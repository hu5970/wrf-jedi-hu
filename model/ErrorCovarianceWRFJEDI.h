/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_ERRORCOVARIANCEWRFJEDI_H_
#define WRFJEDI_MODEL_ERRORCOVARIANCEWRFJEDI_H_

#include <ostream>
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

#include "Fortran.h"
#include "GeometryWRFJEDI.h"
#include "eckit/config/Configuration.h"
#include "oops/util/DateTime.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

// Forward declarations
namespace oops {
  class Variables;
}

namespace wrfjedi {
  class IncrementWRFJEDI;
  class StateWRFJEDI;

// -----------------------------------------------------------------------------
/// Background error covariance matrix for LFRic

class ErrorCovarianceWRFJEDI : public util::Printable,
                             private boost::noncopyable,
                             private util::ObjectCounter<ErrorCovarianceWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::ErrorCovarianceWRFJEDI";}

  ErrorCovarianceWRFJEDI(const GeometryWRFJEDI &, const oops::Variables &,
                       const eckit::Configuration &, const StateWRFJEDI &);
  ~ErrorCovarianceWRFJEDI();

  void linearize(const StateWRFJEDI &, const GeometryWRFJEDI &);
  void multiply(const IncrementWRFJEDI &, IncrementWRFJEDI &) const;
  void inverseMultiply(const IncrementWRFJEDI &, IncrementWRFJEDI &) const;
  void randomize(IncrementWRFJEDI &) const;

 private:
  void print(std::ostream &) const;
  F90bmat keyFtnConfig_;
  boost::scoped_ptr<const GeometryWRFJEDI> geom_;
  util::DateTime time_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi
#endif  // WRFJEDI_MODEL_ERRORCOVARIANCEWRFJEDI_H_
