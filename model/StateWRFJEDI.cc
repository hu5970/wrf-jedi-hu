/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "StateWRFJEDI.h"

#include <algorithm>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "oops/util/Logger.h"
#include "ufo/GeoVaLs.h"
#include "ioda/Locations.h"
#include "ModelBiasWRFJEDI.h"
#include "FieldsWRFJEDI.h"
#include "GeometryWRFJEDI.h"
#include "GetValuesTrajWRFJEDI.h"
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
StateWRFJEDI::StateWRFJEDI(const GeometryWRFJEDI & resol, const oops::Variables & vars,
                           const eckit::Configuration & file)
  : fields_(new FieldsWRFJEDI(resol, vars, util::DateTime())), stash_()
{
  if (file.has("analytic_init"))
    fields_->analytic_init(file, resol);
  else
    fields_->read(file);

  ASSERT(fields_);

  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI created and read in." << std::endl;
}
// -----------------------------------------------------------------------------
StateWRFJEDI::StateWRFJEDI(const GeometryWRFJEDI & resol, const StateWRFJEDI & other)
  : fields_(new FieldsWRFJEDI(*other.fields_, resol)), stash_()
{
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI create by interpolation."
                     << std::endl;
  ASSERT(fields_);
  oops::Log::trace() << "StateWRFJEDI::StateWRFJEDI created by interpolation." 
                     << std::endl;
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
/// Get state values at observation locations
// -----------------------------------------------------------------------------
void StateWRFJEDI::getValues(const ioda::Locations & locs,
                             const oops::Variables & vars,
                             ufo::GeoVaLs & cols) const {
  oops::Log::trace() << "StateWRFJEDI::getValues STANDARD ONE" << std::endl;
  fields_->getValues(locs, vars, cols);
}
// -----------------------------------------------------------------------------
void StateWRFJEDI::getValues(const ioda::Locations & locs,
                             const oops::Variables & vars,
                             ufo::GeoVaLs & cols,
                             const GetValuesTrajWRFJEDI & traj) const {
  oops::Log::trace() << "StateWRFJEDI::getValues PPTRAJ" << std::endl;
  fields_->getValues(locs, vars, cols, traj);
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
/// I/O and diagnostics
// -----------------------------------------------------------------------------
void StateWRFJEDI::read(const eckit::Configuration & files) {
  fields_->read(files);
}
// -----------------------------------------------------------------------------
void StateWRFJEDI::analytic_init(const eckit::Configuration & files,
                                 const GeometryWRFJEDI & resol) {
  fields_->analytic_init(files, resol);
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
