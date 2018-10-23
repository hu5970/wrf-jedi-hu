
#include "RunWRFJEDI.h"

#include <fstream>

#include "eckit/config/Configuration.h"
#include "Fortran.h"
#include "oops/runs/Run.h"
//#include "oops/util/Logger.h"
#include "/vagrant_data/code/wrf-bundle/oops/src/oops/util/Logger.h"

namespace wrfjedi {

// -----------------------------------------------------------------------------

RunWRFJEDI::RunWRFJEDI(int argc, char ** argv) : oops::Run(argc, argv) {
  oops::Log::trace() << "Creating RunWRFJEDI" << std::endl;
  wrfjedi_run_init_f90();
  oops::Log::trace() << "RunWRFJEDI created" << std::endl;
}

// -----------------------------------------------------------------------------

RunWRFJEDI::~RunWRFJEDI() {
  oops::Log::trace() << "Destructing RunWRFJEDI" << std::endl;
  wrfjedi_run_final_f90();
  oops::Log::trace() << "RunWRFJEDI: MPI finalized" << std::endl;
}

// -----------------------------------------------------------------------------

}  // namespace wrfjedi
