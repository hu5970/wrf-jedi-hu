
#include "RunWRFJEDI.h"

#include "Fortran.h"
#include "util/Logger.h"
#include "oops/runs/Run.h"
#include "eckit/config/Configuration.h"

namespace wrfjedi {

// -----------------------------------------------------------------------------

RunWRFJEDI::RunWRFJEDI(int argc, char ** argv) : oops::Run(argc, argv) {
  oops::Log::trace() << "Creating RunWRFJEDI" << std::endl;
  const eckit::Configuration * conf = &config();
  // wrfjedi_init_f90(&conf);
  oops::Log::trace() << "RunWRFJEDI created" << std::endl;
}

// -----------------------------------------------------------------------------

RunWRFJEDI::~RunWRFJEDI() {
  oops::Log::trace() << "Destructing RunWRFJEDI" << std::endl;
  // mpi_finalize_f90();
  oops::Log::trace() << "RunWRFJEDI: MPI finalized" << std::endl;
}

// -----------------------------------------------------------------------------

}  // namespace wrfjedi
