/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "StateWRFJEDI.h"

#include <algorithm>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "util/Logger.h"
#include "ufo/GeoVaLs.h"
#include "ufo/Locations.h"
#include "ModelBiasWRFJEDI.h"
#include "FieldsWRFJEDI.h"
#include "GeometryWRFJEDI.h"
#include "IncrementWRFJEDI.h"
#include "ModelWRFJEDI.h"
#include "oops/base/Variables.h"
#include "oops/generic/UnstructuredGrid.h"
#include "util/DateTime.h"
#include "util/Duration.h"

namespace wrfjedi {

// -----------------------------------------------------------------------------
/// Constructor, destructor
// -----------------------------------------------------------------------------
StateWRFJEDI::StateWRFJEDI(const GeometryWRFJEDI & resol, const oops::Variables & vars,
                 const util::DateTime & vt)
  : fields_(new FieldsWRFJEDI(resol, vars, vt)), stash_()
{
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI created." << std::endl;
}
// -----------------------------------------------------------------------------
StateWRFJEDI::StateWRFJEDI(const GeometryWRFJEDI & resol, const eckit::Configuration & file)
  : fields_(), stash_()
{
// Should get variables from file. YT
//  eckit::LocalConfiguration modelvars;
//  modelvars.set("variables", "cv");
//  oops::Variables vars(modelvars);

//  eckit::LocalConfiguration modelvars;
//  const std::vector<std::string> vv{"cv"};
//  modelvars.set("variables", vv); 
//  oops::Variables vars(vv);

/*
// Should get variables from file. YT
  const std::vector<std::string> vv{"x","bc"};
  oops::Variables vars(vv);
  fields_.reset(new FieldsQG(resol, vars, util::DateTime()));
  fields_->read(file);

  ASSERT(fields_);
  Log::trace() << "StateQG::StateQG created and read in." << std::endl;

 */
// Should get variables from file. YT

// WORKING WITHOUT READING THE NAMELIST AS QG/
  oops::Log::trace() << "StateWRFJEDI::GD0 enforcing to variable to cv" << std::endl;
//  --- For Interface ---
//  const std::vector<std::string> vv{"theta", "rho", "index_qv", "uReconstructZonal", "uReconstructMeridional"};
//  --- For HofX ---
//  const std::vector<std::string> vv{"theta", "index_qv", "pressure_base"};
//  const std::vector<std::string> vv{"theta", "rho", "index_qv", "uReconstructZonal", "uReconstructMeridional", "pressure_base"};
//  const std::vector<std::string> vv{"theta", "rho", "index_qv", "uReconstructZonal", "uReconstructMeridional", "pressure"};
//  --- For Dirac ---
//  const std::vector<std::string> vv{"theta"};
//  const std::vector<std::string> vv{"theta", "uReconstructZonal"};
//  const std::vector<std::string> vv{"uReconstructZonal", "theta"};
  const std::vector<std::string> vv{"theta", "rho", "index_qv", "uReconstructZonal", "uReconstructMeridional"};
//  const std::vector<std::string> vv{"theta", "rho", "index_qv", "uReconstructZonal", "uReconstructMeridional", "pressure"};
  oops::Log::trace() << "StateWRFJEDI::GD1 enforcing to variable to cv" << std::endl;
  oops::Variables vars(vv);
  oops::Log::trace() << "StateWRFJEDI::GD2" << std::endl;


  fields_.reset(new FieldsWRFJEDI(resol, vars, util::DateTime()));
  oops::Log::trace() << "StateWRFJEDI::GD3 before read" << std::endl;
  fields_->read(file);
  oops::Log::trace() << "StateWRFJEDI::GD3 after read" << std::endl;

  ASSERT(fields_);

  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI created and read in." << std::endl;
}
// -----------------------------------------------------------------------------
StateWRFJEDI::StateWRFJEDI(const GeometryWRFJEDI & resol, const StateWRFJEDI & other)
  : fields_(new FieldsWRFJEDI(*other.fields_, resol)), stash_()
{
  ASSERT(fields_);
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI created by interpolation." << std::endl;
}
// -----------------------------------------------------------------------------
StateWRFJEDI::StateWRFJEDI(const StateWRFJEDI & other)
  : fields_(new FieldsWRFJEDI(*other.fields_)), stash_()
{
  
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI before copied." << std::endl;
  ASSERT(fields_);
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI copied." << std::endl;
}
// -----------------------------------------------------------------------------
StateWRFJEDI::~StateWRFJEDI() {
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI destructed." << std::endl;
}
// -----------------------------------------------------------------------------
/// Basic operators
// -----------------------------------------------------------------------------
StateWRFJEDI & StateWRFJEDI::operator=(const StateWRFJEDI & rhs) {
  ASSERT(fields_);
  *fields_ = *rhs.fields_;
  return *this;
}
// -----------------------------------------------------------------------------
/// Interpolate to observation location
// -----------------------------------------------------------------------------
void StateWRFJEDI::interpolate(const ufo::Locations & locs, const oops::Variables & vars, ufo::GeoVaLs & cols) const {
  fields_->interpolate(locs, vars, cols);
}
// -----------------------------------------------------------------------------
/// Interpolate full fields
// -----------------------------------------------------------------------------
void StateWRFJEDI::changeResolution(const StateWRFJEDI & other) {
  fields_->changeResolution(*other.fields_);
  oops::Log::trace() << "StateWRFJEDI interpolated" << std::endl;
}
// -----------------------------------------------------------------------------
/// Interactions with Increments
// -----------------------------------------------------------------------------
StateWRFJEDI & StateWRFJEDI::operator+=(const IncrementWRFJEDI & dx) {
  ASSERT(this->validTime() == dx.validTime());
  ASSERT(fields_);
  fields_->add(dx.fields());
  return *this;
}
// -----------------------------------------------------------------------------
/// Convert to/from unstructured grid
// -----------------------------------------------------------------------------
void StateWRFJEDI::convert_to(oops::UnstructuredGrid & ug) const {
  fields_->convert_to(ug);
}
// -----------------------------------------------------------------------------
void StateWRFJEDI::convert_from(const oops::UnstructuredGrid & ug) {
  fields_->convert_from(ug);
}
// -----------------------------------------------------------------------------
/// I/O and diagnostics
// -----------------------------------------------------------------------------
void StateWRFJEDI::read(const eckit::Configuration & files) {
  fields_->read(files);
}
// -----------------------------------------------------------------------------
void StateWRFJEDI::write(const eckit::Configuration & files) const {
  fields_->write(files);
}
// -----------------------------------------------------------------------------
void StateWRFJEDI::print(std::ostream & os) const {
  os << std::endl << "  Valid time: " << validTime();
  os << *fields_;
}
// -----------------------------------------------------------------------------
/// For accumulator
// -----------------------------------------------------------------------------
void StateWRFJEDI::zero() {
  fields_->zero();
}
// -----------------------------------------------------------------------------
void StateWRFJEDI::accumul(const double & zz, const StateWRFJEDI & xx) {
  fields_->axpy(zz, *xx.fields_);
}
// -----------------------------------------------------------------------------

}  // namespace wrfjedi
