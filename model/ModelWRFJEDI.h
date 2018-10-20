/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_MODELWRFJEDI_H_
#define WRFJEDI_MODEL_MODELWRFJEDI_H_

#include <ostream>
#include <string>

#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

#include "Fortran.h"
#include "GeometryWRFJEDI.h"
#include "oops/base/ModelBase.h"
#include "oops/base/Variables.h"
#include "oops/util/Duration.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace wrfjedi {
  class ModelBiasWRFJEDI;
  class FieldsWRFJEDI;
  class StateWRFJEDI;

// -----------------------------------------------------------------------------
/// WRFJEDI model definition.
/*!
 *  WRFJEDI nonlinear model definition and configuration parameters.
 */

class ModelWRFJEDI: public oops::ModelBase<WRFJEDITraits>,
               private util::ObjectCounter<ModelWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::ModelWRFJEDI";}

  ModelWRFJEDI(const GeometryWRFJEDI &, const eckit::Configuration &);
  ~ModelWRFJEDI();

/// Prepare model integration
  void initialize(StateWRFJEDI &) const;

/// Model integration
  void step(StateWRFJEDI &, const ModelBiasWRFJEDI &) const;
  int saveTrajectory(StateWRFJEDI &, const ModelBiasWRFJEDI &) const;

/// Finish model integration
  void finalize(StateWRFJEDI &) const;

/// Utilities
  const util::Duration & timeResolution() const {return tstep_;}
  const oops::Variables & variables() const {return vars_;}

 private:
  void print(std::ostream &) const;
  F90model keyConfig_;
  util::Duration tstep_;
  const GeometryWRFJEDI geom_;
  const oops::Variables vars_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi
#endif  // WRFJEDI_MODEL_MODELWRFJEDI_H_
