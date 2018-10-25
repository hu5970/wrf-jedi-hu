/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */


#include <vector>

#include "eckit/config/Configuration.h"

#include "FieldsWRFJEDI.h"
#include "Fortran.h"
#include "GeometryWRFJEDI.h"
#include "ModelWRFJEDI.h"
#include "ModelBiasWRFJEDI.h"
#include "StateWRFJEDI.h"

#include "oops/util/DateTime.h"
#include "oops/util/Logger.h"

namespace wrfjedi {
// -----------------------------------------------------------------------------
 static oops::ModelMaker<WRFJEDITraits, ModelWRFJEDI> makermodel_("WRFJEDI");
// -----------------------------------------------------------------------------
ModelWRFJEDI::ModelWRFJEDI(const GeometryWRFJEDI & resol, 
                           const eckit::Configuration & model)
  : keyConfig_(0), tstep_(0), geom_(resol),
    vars_(std::vector<std::string>{"temperature", "pressure", "index_qv",
                              "uReconstructZonal", "uReconstructMeridional"})
{
  oops::Log::trace() << "ModelWRFJEDI::ModelWRFJEDI" << std::endl;
  tstep_ = util::Duration(model.getString("tstep"));
  oops::Log::trace() << "ModelWRFJEDI::tstep_"<<tstep_<<std::endl;
  const eckit::Configuration * configc = &model;
  wrfjedi_model_setup_f90(&configc, geom_.toFortran(), keyConfig_);
  oops::Log::trace() << "ModelWRFJEDI created" << std::endl;
}
// -----------------------------------------------------------------------------
ModelWRFJEDI::~ModelWRFJEDI() {
  wrfjedi_model_delete_f90(keyConfig_);
  oops::Log::trace() << "ModelWRFJEDI destructed" << std::endl;
}
// -----------------------------------------------------------------------------
void ModelWRFJEDI::initialize(StateWRFJEDI & xx) const {
  wrfjedi_model_prepare_integration_f90(keyConfig_, xx.fields().toFortran());
  oops::Log::debug() << "ModelWRFJEDI::initialize" << xx.fields() << std::endl;
}
// -----------------------------------------------------------------------------
void ModelWRFJEDI::step(StateWRFJEDI & xx, const ModelBiasWRFJEDI &) const {
  util::DateTime * dtp = & xx.validTime();
  util::DateTime  dtp_tmp;
  util::DateTime * dtp_next;
  oops::Log::debug() << "ModelWRFJEDI::step fields in" << xx.fields() << std::endl;
  dtp_tmp = xx.validTime() + tstep_;
  dtp_next = & dtp_tmp;
  wrfjedi_model_propagate_f90(keyConfig_, xx.fields().toFortran(), &dtp, &dtp_next);
  xx.validTime() += tstep_;
  oops::Log::debug() << "ModelWRFJEDI::step fields out" << xx.fields() << std::endl;
}
// -----------------------------------------------------------------------------
void ModelWRFJEDI::finalize(StateWRFJEDI & xx) const {
  oops::Log::debug() << "ModelWRFJEDI::finalize" << xx.fields() << std::endl;
}
// -----------------------------------------------------------------------------
int ModelWRFJEDI::saveTrajectory(StateWRFJEDI & xx, const ModelBiasWRFJEDI &) const {
  int ftraj = 0;
  oops::Log::debug() << "ModelWRFJEDI::saveTrajectory fields in" << xx.fields() 
                     << std::endl;
  wrfjedi_model_prop_traj_f90(keyConfig_, xx.fields().toFortran(), ftraj);
  ASSERT(ftraj != 0);
  oops::Log::debug() << "ModelWRFJEDI::saveTrajectory fields out" << xx.fields() 
                     << std::endl;
  return ftraj;
}
// -----------------------------------------------------------------------------
void ModelWRFJEDI::print(std::ostream & os) const {
  os << "ModelWRFJEDI::print not implemented";
}
// -----------------------------------------------------------------------------
}  // namespace wrfjedi
