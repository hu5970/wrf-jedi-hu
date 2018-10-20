/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_MODELBIASINCREMENTWRFJEDI_H_
#define WRFJEDI_MODEL_MODELBIASINCREMENTWRFJEDI_H_

#include <iostream>

#include "oops/util/Printable.h"

namespace eckit {
  class Configuration;
}

namespace wrfjedi {
  class ModelBiasWRFJEDI;
  class ModelBiasCovarianceWRFJEDI;
  class GeometryWRFJEDI;

// -----------------------------------------------------------------------------

class ModelBiasIncrementWRFJEDI : public util::Printable {
 public:
/// Constructor, destructor
  ModelBiasIncrementWRFJEDI(const GeometryWRFJEDI &, const eckit::Configuration &) {}
  ModelBiasIncrementWRFJEDI(const ModelBiasIncrementWRFJEDI &, const bool) {}
  ModelBiasIncrementWRFJEDI(const ModelBiasIncrementWRFJEDI &, 
                            const eckit::Configuration &) {}
  ~ModelBiasIncrementWRFJEDI() {}

/// Linear algebra operators
  void diff(const ModelBiasWRFJEDI &, const ModelBiasWRFJEDI &) {}
  void zero() {}
  ModelBiasIncrementWRFJEDI & operator=(const ModelBiasIncrementWRFJEDI &) 
                                       {return *this;}
  ModelBiasIncrementWRFJEDI & operator+=(const ModelBiasIncrementWRFJEDI &)
                                        {return *this;}
  ModelBiasIncrementWRFJEDI & operator-=(const ModelBiasIncrementWRFJEDI &)
                                        {return *this;}
  ModelBiasIncrementWRFJEDI & operator*=(const double) {return *this;}
  void axpy(const double, const ModelBiasIncrementWRFJEDI &) {}
  double dot_product_with(const ModelBiasIncrementWRFJEDI &) const {return 0.0;}

/// I/O and diagnostics
  void read(const eckit::Configuration &) {}
  void write(const eckit::Configuration &) const {}
  double norm() const {return 0.0;}

 private:
  explicit ModelBiasIncrementWRFJEDI(const ModelBiasCovarianceWRFJEDI &);
  void print(std::ostream & os) const {}
};

// -----------------------------------------------------------------------------

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_MODELBIASINCREMENTWRFJEDI_H_
