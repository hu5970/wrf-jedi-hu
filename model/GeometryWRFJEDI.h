/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_GEOMETRYWRFJEDI_H_
#define WRFJEDI_MODEL_GEOMETRYWRFJEDI_H_

#include <ostream>
#include <string>

#include "Fortran.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace wrfjedi {

// -----------------------------------------------------------------------------
/// GeometryWRFJEDI handles geometry for WRFJEDI model.

class GeometryWRFJEDI : public util::Printable,
                      private util::ObjectCounter<GeometryWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::GeometryWRFJEDI";}

  explicit GeometryWRFJEDI(const eckit::Configuration &);
  GeometryWRFJEDI(const GeometryWRFJEDI &);
  ~GeometryWRFJEDI();

  F90geom & toFortran() {return keyGeom_;}
  const F90geom & toFortran() const {return keyGeom_;}

 private:
  GeometryWRFJEDI & operator=(const GeometryWRFJEDI &);
  void print(std::ostream &) const;
  F90geom keyGeom_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_GEOMETRYWRFJEDI_H_
