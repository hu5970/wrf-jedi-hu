#ifndef WRFJEDI_RUNWRF_H_
#define WRFJEDI_RUNWRF_H_

#include "oops/runs/Run.h"

namespace wrfjedi {

/*!
 *  RunWRFJEDI encapsulates one WRF/OOPS run.
 */

// -----------------------------------------------------------------------------

class RunWRFJEDI : public oops::Run {
 public:
  RunWRFJEDI(int, char **);
  ~RunWRFJEDI();
};

// -----------------------------------------------------------------------------

}  // namespace wrfjedi
#endif  // WRFJEDI_RUNWRF_H_
