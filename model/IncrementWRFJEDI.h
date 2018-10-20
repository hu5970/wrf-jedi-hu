/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_INCREMENTWRFJEDI_H_
#define WRFJEDI_MODEL_INCREMENTWRFJEDI_H_

#include <ostream>
#include <string>

#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>

#include "FieldsWRFJEDI.h"
#include "GeometryWRFJEDI.h"
#include "oops/base/GeneralizedDepartures.h"
#include "util/DateTime.h"
#include "util/Duration.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"
#include "util/dot_product.h"

namespace eckit {
  class Configuration;
}

namespace ufo {
  class GeoVaLs;
  class Locations;
}

namespace oops {
  class Variables;
  class UnstructuredGrid;
}

namespace wrfjedi {
  class ModelBiasIncrementWRFJEDI;
  class ErrorCovarianceWRFJEDI;
  class StateWRFJEDI;

/// Increment Class: Difference between two states
/*!
 *  Some fields that are present in a State may not be present in
 *  an Increment. The Increment contains everything that is needed by
 *  the tangent-linear and adjoint models.
 */

// -----------------------------------------------------------------------------

class IncrementWRFJEDI : public oops::GeneralizedDepartures,
                       public util::Printable,
                       private util::ObjectCounter<IncrementWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::IncrementWRFJEDI";}

/// Constructor, destructor
  IncrementWRFJEDI(const GeometryWRFJEDI &, const oops::Variables &, const util::DateTime &);
  IncrementWRFJEDI(const GeometryWRFJEDI &, const IncrementWRFJEDI &);
  IncrementWRFJEDI(const IncrementWRFJEDI &, const bool);
  IncrementWRFJEDI(const IncrementWRFJEDI &);
  virtual ~IncrementWRFJEDI();

/// Basic operators
  void diff(const StateWRFJEDI &, const StateWRFJEDI &);
  void zero();
  void zero(const util::DateTime &);
  IncrementWRFJEDI & operator =(const IncrementWRFJEDI &);
  IncrementWRFJEDI & operator+=(const IncrementWRFJEDI &);
  IncrementWRFJEDI & operator-=(const IncrementWRFJEDI &);
  IncrementWRFJEDI & operator*=(const double &);
  void axpy(const double &, const IncrementWRFJEDI &, const bool check = true);
  double dot_product_with(const IncrementWRFJEDI &) const;
  void schur_product_with(const IncrementWRFJEDI &);
  void random();
  void dirac(const eckit::Configuration &);

/// Interpolate to observation location
  void interpolateTL(const ufo::Locations &, const oops::Variables &, ufo::GeoVaLs &) const;
  void interpolateAD(const ufo::Locations &, const oops::Variables &, const ufo::GeoVaLs &);

/// I/O and diagnostics
  void read(const eckit::Configuration &);
  void write(const eckit::Configuration &) const;
  double norm() const {return fields_->norm();}
  const util::DateTime & validTime() const {return fields_->time();}
  util::DateTime & validTime() {return fields_->time();}
  void updateTime(const util::Duration & dt) {fields_->time() += dt;}

/// Convert to/from unstructured grid
  void convert_to(oops::UnstructuredGrid &) const;
  void convert_from(const oops::UnstructuredGrid &);

//Access to fields
  FieldsWRFJEDI & fields() {return *fields_;}
  const FieldsWRFJEDI & fields() const {return *fields_;}

  boost::shared_ptr<const GeometryWRFJEDI> geometry() const {
    return fields_->geometry();
  }

/// Other
  void accumul(const double &, const StateWRFJEDI &);

/// Data
 private:
  void print(std::ostream &) const;
  boost::scoped_ptr<FieldsWRFJEDI> fields_;
  boost::scoped_ptr<FieldsWRFJEDI> stash_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_INCREMENTWRFJEDI_H_
