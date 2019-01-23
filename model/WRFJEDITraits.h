/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef WRFJEDI_MODEL_WRFTRAITS_H_
#define WRFJEDI_MODEL_WRFTRAITS_H_

#include <string>

#include "ErrorCovarianceWRFJEDI.h"
#include "GeometryWRFJEDI.h"
#include "GetValuesTrajWRFJEDI.h"
#include "IncrementWRFJEDI.h"
#include "LocalizationMatrixWRFJEDI.h"
//#include "ModelWRFJEDI.h"
#include "ModelBiasWRFJEDI.h"
#include "ModelBiasIncrementWRFJEDI.h"
#include "ModelBiasCovarianceWRFJEDI.h"
#include "StateWRFJEDI.h"
#include "ufo/GeoVaLs.h"
#include "ioda/ObsSpace.h"
#include "ioda/ObsVector.h"
#include "ufo/LinearObsOperator.h"
#include "ufo/Locations.h"
#include "ufo/ObsBias.h"
#include "ufo/ObsBiasIncrement.h"
#include "ufo/ObsBiasCovariance.h"
#include "ufo/ObsOperator.h"
#include "ufo/LinearObsOperator.h"

namespace wrfjedi {

struct WRFJEDITraits {
  static std::string name() {return "WRFJEDI";}
  static std::string nameCovar() {return "WRFJEDIstatic";}

  typedef wrfjedi::GeometryWRFJEDI             Geometry;

  typedef wrfjedi::StateWRFJEDI                State;
  typedef wrfjedi::IncrementWRFJEDI            Increment;
  typedef wrfjedi::ErrorCovarianceWRFJEDI      Covariance;

  //typedef wrfjedi::ModelWRFJEDI                Model;
  typedef wrfjedi::ModelBiasWRFJEDI            ModelAuxControl;
  typedef wrfjedi::ModelBiasIncrementWRFJEDI   ModelAuxIncrement;
  typedef wrfjedi::ModelBiasCovarianceWRFJEDI  ModelAuxCovariance;

  typedef wrfjedi::LocalizationMatrixWRFJEDI   LocalizationMatrix;

  typedef wrfjedi::GetValuesTrajWRFJEDI        InterpolatorTraj;

  typedef ufo::ObsOperator                     ObsOperator;
  typedef ufo::LinearObsOperator               LinearObsOperator;
  typedef ioda::ObsSpace                       ObsSpace;
  typedef ioda::ObsVector                      ObsVector;

  typedef ufo::ObsBias                         ObsAuxControl;
  typedef ufo::ObsBiasIncrement                ObsAuxIncrement;
  typedef ufo::ObsBiasCovariance               ObsAuxCovariance;
//  typedef ufo::ObsCheck                        ObsCheck;

  typedef ufo::GeoVaLs                         GeoVaLs;
  typedef ufo::Locations                       Locations;
};

}  // namespace wrfjedi

#endif  // WRFJEDI_MODEL_WRFTRAITS_H_
