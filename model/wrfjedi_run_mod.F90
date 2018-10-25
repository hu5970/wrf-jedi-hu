module wrfjedi_run_mod

use iso_c_binding 
use module_domain_type, only : domain

implicit none

private

!public :: wrfjedi_head_grid

!type (domain), pointer :: wrfjedi_head_grid => null() 

   INTERFACE

     SUBROUTINE wrfjedi_init(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_init

     SUBROUTINE wrfjedi_run(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_run

     SUBROUTINE wrfjedi_finalize(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_finalize
   END INTERFACE

contains

! ------------------------------------------------------------------------------
subroutine wrfjedi_run_init() bind(c,name='wrfjedi_run_init_f90')

   use fckit_mpi_module, only: fckit_mpi_comm
   USE module_domain, only: program_name
   USE module_symbols_util, ONLY: wrfu_cal_gregorian,WRFU_Initialize

   implicit none
   INTEGER :: mpi_comm_here
   type(fckit_mpi_comm) :: f_comm
   INTEGER :: ierr
   logical :: mpi_inited
   logical :: no_init1
   CHARACTER (LEN=10) :: release_version = 'V4.0      '
!
   f_comm = fckit_mpi_comm()
!   write(*,*) 'fckit_mpi_comm=',f_comm%communicator()
 
   program_name = "WRF " // TRIM(release_version) // " MODEL"

   CALL MPI_INITIALIZED(mpi_inited, ierr)
   if(mpi_inited) then
      mpi_comm_here = f_comm%communicator()
      CALL wrf_set_dm_communicator( mpi_comm_here )
      CALL wrf_termio_dup( mpi_comm_here )
   endif

   no_init1=.true.
   CALL init_modules(1)
   IF ( no_init1  ) THEN
     CALL WRFU_Initialize( defaultCalKind=WRFU_CAL_GREGORIAN )
   ENDIF

   CALL init_modules(2)
   
!   call wrfjedi_init( wrfjedi_head_grid ) 
!   call wrfjedi_run( wrfjedi_head_grid )

end subroutine wrfjedi_run_init

! ------------------------------------------------------------------------------
!!
subroutine wrfjedi_run_final() bind(c,name='wrfjedi_run_final_f90')

implicit none

!   call wrfjedi_finalize( wrfjedi_head_grid )

end subroutine wrfjedi_run_final

! ------------------------------------------------------------------------------

end module wrfjedi_run_mod
