/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_STATEWRFJEDI_H_
#define WRFJEDI_MODEL_STATEWRFJEDI_H_

#include <ostream>
#include <string>

#include <boost/scoped_ptr.hpp>

#include "FieldsWRFJEDI.h"
#include "util/DateTime.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace ufo {
  class GeoVaLs;
  class Locations;
}

namespace oops {
  class UnstructuredGrid;
  class Variables;
}

namespace wrfjedi {
  class GeometryWRFJEDI;
  class IncrementWRFJEDI;

/// WRFJEDI model state
/*!
 * A State contains everything that is needed to propagate the state
 * forward in time.
 */

// -----------------------------------------------------------------------------
class StateWRFJEDI : public util::Printable,
                private util::ObjectCounter<StateWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::StateWRFJEDI";}

/// Constructor, destructor
  StateWRFJEDI(const GeometryWRFJEDI &, const oops::Variables &, const util::DateTime &);  // Is it used?
  StateWRFJEDI(const GeometryWRFJEDI &, const eckit::Configuration &);
  StateWRFJEDI(const GeometryWRFJEDI &, const StateWRFJEDI &);
  StateWRFJEDI(const StateWRFJEDI &);
  virtual ~StateWRFJEDI();
  StateWRFJEDI & operator=(const StateWRFJEDI &);

/// Interpolate to observation location
  void interpolate(const ufo::Locations &, const oops::Variables &, ufo::GeoVaLs &) const;

/// Interpolate full fields
  void changeResolution(const StateWRFJEDI & xx);

/// Interactions with Increment
  StateWRFJEDI & operator+=(const IncrementWRFJEDI &);

/// I/O and diagnostics
  void read(const eckit::Configuration &);
  void write(const eckit::Configuration &) const;
  double norm() const {return fields_->norm();}
  const util::DateTime & validTime() const {return fields_->time();}
  util::DateTime & validTime() {return fields_->time();}

/// Convert to/from unstructured grid
  void convert_to(oops::UnstructuredGrid &) const;
  void convert_from(const oops::UnstructuredGrid &);

/// Access to fields
  FieldsWRFJEDI & fields() {return *fields_;}
  const FieldsWRFJEDI & fields() const {return *fields_;}

  boost::shared_ptr<const GeometryWRFJEDI> geometry() const {
    return fields_->geometry();
  }

/// Other
  void zero();
  void accumul(const double &, const StateWRFJEDI &);

 private:
  void print(std::ostream &) const;
  boost::scoped_ptr<FieldsWRFJEDI> fields_;
  boost::scoped_ptr<FieldsWRFJEDI> stash_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_STATEWRFJEDI_H_
