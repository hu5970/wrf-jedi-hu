/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_LOCALIZATIONMATRIXWRFJEDI_H_
#define WRFJEDI_MODEL_LOCALIZATIONMATRIXWRFJEDI_H_

#include <ostream>
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>

#include "GeometryWRFJEDI.h"
#include "eckit/config/Configuration.h"
#include "oops/interface/LocalizationBase.h"
#include "util/DateTime.h"
#include "util/ObjectCounter.h"
#include "util/Printable.h"

#include "Fortran.h"

// Forward declarations
namespace wrfjedi {
  class GeometryWRFJEDI;
  class IncrementWRFJEDI;

/// Localization matrix for WRFJEDI model.

// -----------------------------------------------------------------------------
class LocalizationMatrixWRFJEDI: public util::Printable,
                            private boost::noncopyable,
                            private util::ObjectCounter<LocalizationMatrixWRFJEDI> {
 public:
  static const std::string classname() {return "wrfjedi::LocalizationMatrixWRFJEDI";}

  LocalizationMatrixWRFJEDI(const GeometryWRFJEDI &, const eckit::Configuration &);
  ~LocalizationMatrixWRFJEDI();
  void multiply(IncrementWRFJEDI &) const;

 private:
  void print(std::ostream &) const;
  F90lclz keyFtnConfig_;
};
// -----------------------------------------------------------------------------

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_LOCALIZATIONMATRIXWRFJEDI_H_
