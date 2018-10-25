/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "FieldsWRFJEDI.h"

#include <cmath>
#include <map>
#include <string>
#include <vector>

#include "eckit/config/Configuration.h"
#include "oops/base/Variables.h"
#include "oops/generic/UnstructuredGrid.h"
#include "ufo/GeoVaLs.h"
#include "ioda/Locations.h"
#include "oops/util/Logger.h"
#include "Fortran.h"
#include "GeometryWRFJEDI.h"
#include "GetValuesTrajWRFJEDI.h"
#include "oops/util/DateTime.h"

// -----------------------------------------------------------------------------
namespace wrfjedi {
// -----------------------------------------------------------------------------
FieldsWRFJEDI::FieldsWRFJEDI(const GeometryWRFJEDI & geom, const oops::Variables & vars,
                         const util::DateTime & time):
  geom_(new GeometryWRFJEDI(geom)), vars_(vars), time_(time)
{
  const eckit::Configuration * conf = &vars_.toFortran();
  wrfjedi_field_create_f90(keyFlds_, geom_->toFortran(), &conf);
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI::FieldsWRFJEDI(const FieldsWRFJEDI & other, const bool copy)
  : geom_(other.geom_), vars_(other.vars_), time_(other.time_)
{
  const eckit::Configuration * conf = &vars_.toFortran();
  wrfjedi_field_create_f90(keyFlds_, geom_->toFortran(), &conf);
  if (copy) {
    wrfjedi_field_copy_f90(keyFlds_, other.keyFlds_);
  } else {
    wrfjedi_field_zero_f90(keyFlds_);
  }
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI::FieldsWRFJEDI(const FieldsWRFJEDI & other)
  : geom_(other.geom_), vars_(other.vars_), time_(other.time_)
{
  const eckit::Configuration * conf = &vars_.toFortran();
  wrfjedi_field_create_f90(keyFlds_, geom_->toFortran(), &conf);
  wrfjedi_field_copy_f90(keyFlds_, other.keyFlds_);
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI::FieldsWRFJEDI(const FieldsWRFJEDI & other, const GeometryWRFJEDI & geom)
  : geom_(new GeometryWRFJEDI(geom)), vars_(other.vars_), time_(other.time_)
{
  const eckit::Configuration * conf = &vars_.toFortran();
  wrfjedi_field_create_f90(keyFlds_, geom_->toFortran(), &conf);
  wrfjedi_field_change_resol_f90(keyFlds_, other.keyFlds_);
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI::FieldsWRFJEDI(const FieldsWRFJEDI & other, const oops::Variables & vars)
  : geom_(other.geom_), vars_(vars), time_(other.time_)
{
  const eckit::Configuration * conf = &vars_.toFortran();
  wrfjedi_field_create_f90(keyFlds_, geom_->toFortran(), &conf);
  wrfjedi_field_copy_f90(keyFlds_, other.keyFlds_);
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI::~FieldsWRFJEDI() {
  wrfjedi_field_delete_f90(keyFlds_);
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI & FieldsWRFJEDI::operator=(const FieldsWRFJEDI & rhs) {
  wrfjedi_field_copy_f90(keyFlds_, rhs.keyFlds_);
  time_ = rhs.time_;
  return *this;
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI & FieldsWRFJEDI::operator+=(const FieldsWRFJEDI & rhs) {
  wrfjedi_field_self_add_f90(keyFlds_, rhs.keyFlds_);
  return *this;
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI & FieldsWRFJEDI::operator-=(const FieldsWRFJEDI & rhs) {
  wrfjedi_field_self_sub_f90(keyFlds_, rhs.keyFlds_);
  return *this;
}
// -----------------------------------------------------------------------------
FieldsWRFJEDI & FieldsWRFJEDI::operator*=(const double & zz) {
  wrfjedi_field_self_mul_f90(keyFlds_, zz);
  return *this;
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::zero() {
  wrfjedi_field_zero_f90(keyFlds_);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::zero(const util::DateTime & time) {
  wrfjedi_field_zero_f90(keyFlds_);
  time_ = time;
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::axpy(const double & zz, const FieldsWRFJEDI & rhs) {
  wrfjedi_field_axpy_f90(keyFlds_, zz, rhs.keyFlds_);
}
// -----------------------------------------------------------------------------
double FieldsWRFJEDI::dot_product_with(const FieldsWRFJEDI & fld2) const {
  double zz;
  wrfjedi_field_dot_prod_f90(keyFlds_, fld2.keyFlds_, zz);
  return zz;
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::schur_product_with(const FieldsWRFJEDI & dx) {
    wrfjedi_field_self_schur_f90(keyFlds_, dx.keyFlds_);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::random() {
  wrfjedi_field_random_f90(keyFlds_);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::getValues(const ioda::Locations & locs,
                           const oops::Variables & vars,
                           ufo::GeoVaLs & gom) const {
  const eckit::Configuration * conf = &vars.toFortran();
  wrfjedi_field_getvalues_notraj_f90(keyFlds_, locs.toFortran(), &conf,
                                 gom.toFortran());
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::getValues(const ioda::Locations & locs,
                           const oops::Variables & vars,
                           ufo::GeoVaLs & gom,
                           const GetValuesTrajWRFJEDI & traj) const {
  const eckit::Configuration * conf = &vars.toFortran();
  wrfjedi_field_getvalues_f90(keyFlds_, locs.toFortran(), &conf,
                           gom.toFortran(), traj.toFortran());
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::getValuesTL(const ioda::Locations & locs,
                             const oops::Variables & vars,
                             ufo::GeoVaLs & gom,
                             const GetValuesTrajWRFJEDI & traj) const {
  const eckit::Configuration * conf = &vars.toFortran();
  wrfjedi_field_getvalues_tl_f90(keyFlds_, locs.toFortran(), &conf,
                              gom.toFortran(), traj.toFortran());
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::getValuesAD(const ioda::Locations & locs,
                             const oops::Variables & vars,
                             const ufo::GeoVaLs & gom,
                             const GetValuesTrajWRFJEDI & traj) {
  const eckit::Configuration * conf = &vars.toFortran();
  wrfjedi_field_getvalues_ad_f90(keyFlds_, locs.toFortran(), &conf,
                              gom.toFortran(), traj.toFortran());
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::changeResolution(const FieldsWRFJEDI & other) {
  wrfjedi_field_change_resol_f90(keyFlds_, other.keyFlds_);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::add(const FieldsWRFJEDI & rhs) {
  wrfjedi_field_add_incr_f90(keyFlds_, rhs.keyFlds_);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::diff(const FieldsWRFJEDI & x1, const FieldsWRFJEDI & x2) {
  wrfjedi_field_diff_incr_f90(keyFlds_, x1.keyFlds_, x2.keyFlds_);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::ug_coord(oops::UnstructuredGrid & ug,
                          const int & colocated) const {
  wrfjedi_field_ug_coord_f90(keyFlds_, ug.toFortran(), colocated);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::field_to_ug(oops::UnstructuredGrid & ug,
                             const int & colocated) const {
  wrfjedi_field_field_to_ug_f90(keyFlds_, ug.toFortran(), colocated);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::field_from_ug(const oops::UnstructuredGrid & ug) {
  wrfjedi_field_field_from_ug_f90(keyFlds_, ug.toFortran());
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::read(const eckit::Configuration & config) {
  const eckit::Configuration * conf = &config;
  util::DateTime * dtp = &time_;
  wrfjedi_field_read_file_f90(keyFlds_, &conf, &dtp);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::analytic_init(const eckit::Configuration & config,
                                  const GeometryWRFJEDI & geom) {
  const eckit::Configuration * conf = &config;
  util::DateTime * dtp = &time_;
// JJG: Need to check if geometry is initialized before this!!!
//  wrfjedi_field_analytic_init_f90(keyFlds_, geom.toFortran(), &conf, &dtp);
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::write(const eckit::Configuration & config) const {
  const eckit::Configuration * conf = &config;
  const util::DateTime * dtp = &time_;
  wrfjedi_field_write_file_f90(keyFlds_, &conf, &dtp);
}
// -----------------------------------------------------------------------------
double FieldsWRFJEDI::norm() const {
  double zz = 0.0;
  wrfjedi_field_rms_f90(keyFlds_, zz);
  return zz;
}
// -----------------------------------------------------------------------------
void FieldsWRFJEDI::print(std::ostream & os) const {
// TODO: implement this
//  wrfjedi_field_sizes_f90(keyFlds_, nx, ny, nf, nb);
  int nx = 1;
  int ny = 1;
  int nf = 5;
  int nb = 1;
  os << std::endl << "  Resolution = " << nx << ", " << ny
     << ", Fields = " << nf << ", " << nb;
  nf += nb;
  std::vector<double> zstat(3*nf);
  wrfjedi_field_gpnorm_f90(keyFlds_, nf, zstat[0]);
  for (int jj = 0; jj < nf; ++jj) {
    os << std::endl << "  Min=" << zstat[3*jj]
       << ", Max=" << zstat[3*jj+1] << ", RMS=" << zstat[3*jj+2];
  }
}
// -----------------------------------------------------------------------------
}  // namespace wrfjedi
