module wrfjedi_run_mod

use iso_c_binding 
use module_domain_type, only : domain

implicit none

private

public :: wrfjedi_head_grid

type (domain), pointer :: wrfjedi_head_grid => null() 

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

   implicit none
   INTEGER :: mpi_comm_here
   type(fckit_mpi_comm) :: f_comm
   INTEGER :: ierr
   logical :: mpi_inited
!
   f_comm = fckit_mpi_comm()
!   write(*,*) 'fckit_mpi_comm=',f_comm%communicator()

   CALL MPI_INITIALIZED(mpi_inited, ierr)
   if(mpi_inited) then
      mpi_comm_here = f_comm%communicator()
      CALL wrf_set_dm_communicator( mpi_comm_here )
      CALL wrf_termio_dup( mpi_comm_here )
   endif

   call wrfjedi_init( wrfjedi_head_grid ) 
   call wrfjedi_run( wrfjedi_head_grid )

end subroutine wrfjedi_run_init

! ------------------------------------------------------------------------------
!!
subroutine wrfjedi_run_final() bind(c,name='wrfjedi_run_final_f90')

implicit none

   call wrfjedi_finalize( wrfjedi_head_grid )

end subroutine wrfjedi_run_final

! ------------------------------------------------------------------------------

end module wrfjedi_run_mod
