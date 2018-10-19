/*
 * (C) Copyright 2017 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "util/Logger.h"
#include "GeometryWRFJEDI.h"
#include "Fortran.h"
#include "eckit/config/Configuration.h"

// -----------------------------------------------------------------------------
namespace wrfjedi {
// -----------------------------------------------------------------------------
GeometryWRFJEDI::GeometryWRFJEDI(const eckit::Configuration & conf) {
  const eckit::Configuration * configc = &conf;
  oops::Log::trace() << "============ GeometryWRFJEDI::GeometryWRFJEDI step 1 =============" << std::endl;
  //wrfjedi_geo_setup_f90(keyGeom_, &configc);
  oops::Log::trace() << "============ GeometryWRFJEDI::GeometryWRFJEDI step 2 =============" << std::endl;
}
// -----------------------------------------------------------------------------
GeometryWRFJEDI::GeometryWRFJEDI(const GeometryWRFJEDI & other) {
  const int key_geo = other.keyGeom_;
  oops::Log::trace() << "============ GeometryWRFJEDI wrfjedi_geo_clone_f90   =============" << std::endl;
  //wrfjedi_geo_clone_f90(key_geo, keyGeom_);
}
// -----------------------------------------------------------------------------
GeometryWRFJEDI::~GeometryWRFJEDI() {
  //wrfjedi_geo_delete_f90(keyGeom_);
}
// -----------------------------------------------------------------------------
void GeometryWRFJEDI::print(std::ostream & os) const {
  int nCells;
  int nEdges;
  int nVertLevels;
  int nVertLevelsP1;
  //wrfjedi_geo_info_f90(keyGeom_,nCells, nEdges, nVertLevels, nVertLevelsP1);
  os << "nCells = " << nCells << ", nEdges = " << nEdges <<", nVertLevels = "<<nVertLevels<<", nVertLevelsP1 = "<<nVertLevelsP1 ;
}
// -----------------------------------------------------------------------------
}  // namespace wrfjedi
