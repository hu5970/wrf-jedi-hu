/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_MODELBIASCOVARIANCEWRFJEDI_H_
#define WRFJEDI_MODEL_MODELBIASCOVARIANCEWRFJEDI_H_

#include <ostream>
#include <string>
#include <boost/noncopyable.hpp>

#include "eckit/config/LocalConfiguration.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

namespace wrfjedi {
  class ModelBiasWRFJEDI;
  class ModelBiasIncrementWRFJEDI;
  class GeometryWRFJEDI;

// -----------------------------------------------------------------------------

class ModelBiasCovarianceWRFJEDI : public util::Printable,
                                 private boost::noncopyable,
                                 private util::ObjectCounter<ModelBiasCovarianceWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::ModelBiasCovarianceWRFJEDI";}

/// Constructor, destructor
  ModelBiasCovarianceWRFJEDI(const eckit::Configuration & conf, const GeometryWRFJEDI &): conf_(conf) {}
  ~ModelBiasCovarianceWRFJEDI() {}

/// Linear algebra operators
  void linearize(const ModelBiasWRFJEDI &, const GeometryWRFJEDI &) {}
  void multiply(const ModelBiasIncrementWRFJEDI &, ModelBiasIncrementWRFJEDI) const {}
  void inverseMultiply(const ModelBiasIncrementWRFJEDI &, ModelBiasIncrementWRFJEDI) const {}
  void randomize(ModelBiasIncrementWRFJEDI &) const {}

  const eckit::Configuration & config() const {return conf_;}

 private:
  void print(std::ostream & os) const {}
  const eckit::LocalConfiguration conf_;
};

// -----------------------------------------------------------------------------

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_MODELBIASCOVARIANCEWRFJEDI_H_
