/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_FIELDSWRFJEDI_H_
#define WRFJEDI_MODEL_FIELDSWRFJEDI_H_

#include <ostream>
#include <string>

#include <boost/shared_ptr.hpp>

#include "GeometryWRFJEDI.h"
#include "GetValuesTrajWRFJEDI.h"
#include "oops/base/Variables.h"
#include "oops/util/DateTime.h"
#include "oops/util/Duration.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace oops {
  class UnstructuredGrid;
//  class Variables;
}

namespace ufo {
  class GeoVaLs;
}

namespace ioda {
  class Locations;
}

namespace wrfjedi {
// -----------------------------------------------------------------------------
/// Class to represent a FieldSet for the WRFJEDI model
class FieldsWRFJEDI : public util::Printable,
                    private util::ObjectCounter<FieldsWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::FieldsWRFJEDI";}

// Constructors and basic operators
  FieldsWRFJEDI(const GeometryWRFJEDI &, const oops::Variables &, 
                const util::DateTime &);
  FieldsWRFJEDI(const FieldsWRFJEDI &, const GeometryWRFJEDI &);
  FieldsWRFJEDI(const FieldsWRFJEDI &, const oops::Variables &);
  FieldsWRFJEDI(const FieldsWRFJEDI &, const bool);
  FieldsWRFJEDI(const FieldsWRFJEDI &);
  ~FieldsWRFJEDI();

  void zero();
  void zero(const util::DateTime &);
  FieldsWRFJEDI & operator=(const FieldsWRFJEDI &);
  FieldsWRFJEDI & operator+=(const FieldsWRFJEDI &);
  FieldsWRFJEDI & operator-=(const FieldsWRFJEDI &);
  FieldsWRFJEDI & operator*=(const double &);
  void axpy(const double &, const FieldsWRFJEDI &);
  double dot_product_with(const FieldsWRFJEDI &) const;
  void schur_product_with(const FieldsWRFJEDI &);
  void random();
  void dirac(const eckit::Configuration &);

/// Get state values or increments at observation locations
  void getValues(const ioda::Locations &, const oops::Variables &,
                 ufo::GeoVaLs &) const;
  void getValues(const ioda::Locations &, const oops::Variables &,
                 ufo::GeoVaLs &, const GetValuesTrajWRFJEDI &) const;
  void getValuesTL(const ioda::Locations &, const oops::Variables &,
                   ufo::GeoVaLs &, const GetValuesTrajWRFJEDI &) const;
  void getValuesAD(const ioda::Locations &, const oops::Variables &,
                   const ufo::GeoVaLs &, const GetValuesTrajWRFJEDI &);

// Interpolate full fields
  void changeResolution(const FieldsWRFJEDI &);
  void add(const FieldsWRFJEDI &);
  void diff(const FieldsWRFJEDI &, const FieldsWRFJEDI &);

// Unstructured grid
  void ug_coord(oops::UnstructuredGrid &, const int &) const;
  void field_to_ug(oops::UnstructuredGrid &, const int &) const;
  void field_from_ug(const oops::UnstructuredGrid &);

// Utilities
  void read(const eckit::Configuration &);
  void analytic_init(const eckit::Configuration &, const GeometryMPAS &);
  void write(const eckit::Configuration &) const;
  double norm() const;
  boost::shared_ptr<const GeometryWRFJEDI> geometry() const {return geom_;}

  const util::DateTime & time() const {return time_;}
  util::DateTime & time() {return time_;}

  int & toFortran() {return keyFlds_;}
  const int & toFortran() const {return keyFlds_;}

 private:
  void print(std::ostream &) const;
  F90flds keyFlds_;
  boost::shared_ptr<const GeometryWRFJEDI> geom_;
  oops::Variables vars_;
  util::DateTime time_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi
#endif  // WRFJEDI_MODEL_FIELDSWRFJEDI_H_
