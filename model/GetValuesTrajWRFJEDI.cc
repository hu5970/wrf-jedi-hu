/*
 * (C) Copyright 2018 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "model/GetValuesTrajWRFJEDI.h"
#include "oops/util/Logger.h"
#include "Fortran.h"
#include "eckit/config/Configuration.h"

// -----------------------------------------------------------------------------
namespace wrfjedi {
// -----------------------------------------------------------------------------
GetValuesTrajWRFJEDI::GetValuesTrajWRFJEDI() {
  oops::Log::trace() << "GetValuesTrajWRFJEDI constructor starting"
                     << std::endl;
//  wrfjedi_getvaltraj_setup_f90(keyGetValuesTraj_);
  oops::Log::trace() << "GetValuesTrajWRFJEDI constructor done"
                     << keyGetValuesTraj_ << std::endl;
}
// -----------------------------------------------------------------------------
GetValuesTrajWRFJEDI::~GetValuesTrajWRFJEDI() {
  oops::Log::trace() << "GetValuesTrajWRFJEDI destructor starting"
                     << std::endl;
//  wrfjedi_getvaltraj_delete_f90(keyGetValuesTraj_);
  oops::Log::trace() << "GetValuesTrajWRFJEDI destructor done" << std::endl;
}
// -----------------------------------------------------------------------------
}  // namespace mpas
