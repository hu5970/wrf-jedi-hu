! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module wrfjedi_geom_mod

use iso_c_binding
use wrfjedi_kinds, only : kind_real
USE module_domain_type, only: domain
USE module_configure, only : model_config_rec_type

!use wrfjedi_run_mod, only : run_corelist=>corelist, run_domain=>domain


implicit none
private
public :: wrfjedi_geom, &
        & geo_setup, geo_clone, geo_delete, geo_info
public :: wrfjedi_geom_registry


! ------------------------------------------------------------------------------

!> Fortran derived type to hold geometry definition
type :: wrfjedi_geom
   integer :: max_dom
   integer,DIMENSION(:), ALLOCATABLE :: e_we 
   integer,DIMENSION(:), ALLOCATABLE :: e_sn
   integer,DIMENSION(:), ALLOCATABLE :: e_vert
!   real(kind=kind_real),DIMENSION(:), ALLOCATABLE :: dx
!   real(kind=kind_real),DIMENSION(:), ALLOCATABLE :: dy

   type (domain), pointer :: wrfjedi_head_grid => null() 
   type (model_config_rec_type), pointer :: wrfjedi_model_config_rec => null()
end type wrfjedi_geom

#define LISTED_TYPE wrfjedi_geom

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrfjedi_geom_registry

! ------------------------------------------------------------------------------
!   INTERFACE
!
!     SUBROUTINE wrfjedi_run(wrfjedi_head_grid)
!        USE module_domain_type,   only : domain
!        TYPE(domain), pointer :: wrfjedi_head_grid
!     END SUBROUTINE wrfjedi_run
!
!     SUBROUTINE wrfjedi_finalize(wrfjedi_head_grid)
!        USE module_domain_type,   only : domain
!        TYPE(domain), pointer :: wrfjedi_head_grid
!     END SUBROUTINE wrfjedi_finalize
!   END INTERFACE
!
! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------
subroutine geo_setup(self, c_conf)

   USE module_configure, only : model_config_rec

   implicit none

   type(wrfjedi_geom), intent(inout) :: self
   type(c_ptr), intent(in) :: c_conf

   integer :: max_dom
   integer :: sd1,ed1,sd2,ed2,sd3,ed3
   integer :: i

   write(*,*)' ==> create geom'

   call wrfjedi_geom_init(self%wrfjedi_head_grid,self%wrfjedi_model_config_rec)

   if (associated(self % wrfjedi_head_grid)) then
       write(*,*)'inside geom: geom % wrfjedi_head_grid associated'
   end if
   if (associated(self % wrfjedi_model_config_rec)) then
       write(*,*)'inside geom: geom % wrfjedi_model_config_rec associated'
   else
       write(*,*)'inside geom: geom % wrfjedi_model_config_rec not associated'
   end if
 
! ------------------------------------------------------------------------------
   max_dom = model_config_rec%max_dom

   write(*,*) max_dom, model_config_rec%max_dom

   if(max_dom > 0 ) then
      allocate(self%e_we(max_dom))
      allocate(self%e_sn(max_dom))
      allocate(self%e_vert(max_dom))
   
      do i=1,max_dom
         sd1=model_config_rec%s_we(i)
         ed1=model_config_rec%e_we(i)
         sd2=model_config_rec%s_sn(i)
         ed2=model_config_rec%e_sn(i)
         sd3=model_config_rec%s_vert(i)
         ed3=model_config_rec%e_vert(i)

         self%e_we(i)=ed1-sd1
         self%e_sn(i)=ed2-sd2
         self%e_vert(i)=ed3-sd3
      enddo
!
      write(*,*) 'max domain = ',max_dom
      do i=1,max_dom
         write(*,'(a,4I5)') 'domain=',i,self%e_we(i),self%e_sn(i),self%e_vert(i)
      enddo
   else
      write(*,*) 'wrong max domain = ',max_dom
   endif
   write(*,*)'End of geo_setup'

!   call wrfjedi_run(self%wrfjedi_head_grid)
!   call wrfjedi_finalize(self%wrfjedi_head_grid)

end subroutine geo_setup

! ------------------------------------------------------------------------------

subroutine geo_clone(self, other)

   implicit none

   type(wrfjedi_geom), intent(in) :: self
   type(wrfjedi_geom), intent(inout) :: other

   write(*,*)'====> copy of geom array'

   other % max_dom = self % max_dom

   if (.not.allocated(other % e_we)) allocate(other % e_we(self % max_dom))
   if (.not.allocated(other % e_sn)) allocate(other % e_sn(self % max_dom))
   if (.not.allocated(other % e_vert)) allocate(other % e_vert(self % max_dom))
   other % e_we = self % e_we
   other % e_sn = self % e_sn
   other % e_vert = self % e_vert

   write(*,*)'====> copy of geom wrfjedi_head_grid and wrfjedi_model_config_rec'

   if ((associated(other % wrfjedi_head_grid)).and.(associated(other % wrfjedi_model_config_rec))) then 
      write(*,*)'associated(other % wrfjedi_head_grid), associated(other % wrfjedi_model_config_rec)'
   else
      write(*,*)'not associated(other % wrfjedi_head_grid), associated(other % wrfjedi_model_config_rec)'
      other % wrfjedi_head_grid => self%wrfjedi_head_grid
      other % wrfjedi_model_config_rec => self%wrfjedi_model_config_rec 
   end if

   write(*,*)'====> copy of geom done'

end subroutine geo_clone

! ------------------------------------------------------------------------------

subroutine geo_delete(self)

   implicit none

   type(wrfjedi_geom), intent(inout) :: self

   write(*,*)'==> delete geom array'
   if (allocated(self % e_we)) deallocate(self % e_we)
   if (allocated(self % e_sn)) deallocate(self % e_sn)
   if (allocated(self % e_vert)) deallocate(self % e_vert)

   if ((associated(self % wrfjedi_head_grid)).and.(associated(self % wrfjedi_model_config_rec))) then
      nullify(self % wrfjedi_head_grid)
      nullify(self % wrfjedi_head_grid)
      write(*,*)'==> nullify geom wrfjedi_head_grid and wrfjedi_model_config_rec'
   end if
   write(*,*)'==> delete geom done'

end subroutine geo_delete

! ------------------------------------------------------------------------------

subroutine geo_info(self,domain_id, nx,ny,nz)

   implicit none

   type(wrfjedi_geom), intent(in) :: self
   integer, intent(in) :: domain_id
   integer, intent(inout) :: nx,ny,nz

   nx  = self%e_we(domain_id)
   ny  = self%e_sn(domain_id)
   nz  = self%e_vert(domain_id)

end subroutine geo_info

! ------------------------------------------------------------------------------

 SUBROUTINE wrfjedi_geom_init(head_grid,wrfjedi_model_config_rec)

   USE module_driver_constants
   USE module_timing
   USE module_wrf_error
   USE module_domain_type, only: domain
   USE module_domain, only: program_name,HISTORY_ALARM
   USE module_domain, only: domain_get_stop_time,domain_get_start_time,&
                            alloc_and_configure_domain,domain_get_current_time
   USE module_domain, only: head_grid_dfi_stage,head_grid_id, &
                            head_grid_current_time, &
                            head_grid_start_subtime,head_grid_stop_subtime
   USE module_configure, only : model_config_rec_type
   USE module_configure, only : model_config_rec,grid_config_rec_type
   USE module_configure, only : set_config_as_buffer,get_config_as_buffer,&
                                initial_config,model_to_grid_config_rec
   USE module_check_a_mundo, only : setup_physics_suite,set_physics_rconfigs,&
                                    check_nml_consistency
   USE module_state_description, only: DFI_NODFI,DFI_SETUP
   USE module_symbols_util, ONLY: wrfu_cal_gregorian,WRFU_Initialize
   USE module_dm, ONLY : wrf_dm_initialize,domain_active_this_task,mpi_comm_allcompute

   IMPLICIT NONE

!   TYPE (domain) , pointer :: wrfjedi_head_grid
   TYPE (domain) , pointer :: head_grid
   TYPE (model_config_rec_type), pointer :: wrfjedi_model_config_rec
   TYPE (model_config_rec_type), target :: model_config_rec_tmp

   TYPE (domain) , POINTER :: null_domain
   TYPE (domain) , pointer :: parent_grid 
   TYPE (grid_config_rec_type), SAVE :: config_flags
   INTEGER        :: kid, nestid

   INTEGER :: max_dom , domain_id , fid , oid , idum1 , idum2 , ierr
   INTEGER :: debug_level

   INTEGER                 :: nbytes
   INTEGER, PARAMETER      :: configbuflen = 4* 65536
   INTEGER                 :: configbuf( configbuflen )
   LOGICAL , EXTERNAL      :: wrf_dm_on_monitor

   INTEGER :: save_comm

   INTERFACE 
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain, only: domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE


   CALL wrf_get_dm_communicator( save_comm )
   CALL wrf_set_dm_communicator( mpi_comm_allcompute )
   IF ( wrf_dm_on_monitor() ) THEN
     CALL initial_config
   ENDIF
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_dm_initialize
   CALL wrf_set_dm_communicator( save_comm )

   CALL setup_physics_suite
   CALL set_derived_rconfigs(model_config_rec)
   CALL check_nml_consistency
   CALL set_physics_rconfigs

   CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   NULLIFY( null_domain )

   CALL nl_get_max_dom( 1, max_dom )

   CALL       wrf_message ( program_name )
   CALL       wrf_debug ( 100 , 'wrf: calling alloc_and_configure_domain ' )
   CALL alloc_and_configure_domain ( domain_id  = 1 ,                  &
                               active_this_task = domain_active_this_task(1), &
                                     grid       = head_grid ,          &
                                     parent     = null_domain ,        &
                                     kid        = -1                   )

   CALL       wrf_debug ( 100 , 'wrf: calling model_to_grid_config_rec ' )
   CALL model_to_grid_config_rec ( head_grid%id , model_config_rec , config_flags )
   CALL       wrf_debug ( 100 , 'wrf: calling set_scalar_indices_from_config ' )
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )
   CALL       wrf_debug ( 100 , 'wrf: calling init_wrfio' )
   CALL init_wrfio

   CALL wrf_get_dm_communicator( save_comm )
   CALL wrf_set_dm_communicator( mpi_comm_allcompute )
   CALL get_config_as_buffer( configbuf, configbuflen, nbytes )
   CALL wrf_dm_bcast_bytes( configbuf, nbytes )
   CALL set_config_as_buffer( configbuf, configbuflen )
   CALL wrf_set_dm_communicator( save_comm )
   
   IF ( head_grid%dfi_opt .NE. DFI_NODFI ) head_grid%dfi_stage = DFI_SETUP
   head_grid_dfi_stage=head_grid%dfi_stage
   head_grid_id=head_grid%id

   CALL Setup_Timekeeping (head_grid)
   head_grid_current_time=domain_get_current_time(head_grid)

   IF ( domain_active_this_task(1) ) THEN
      CALL med_initialdata_input( head_grid , config_flags )

      IF ( config_flags%write_restart_at_0h ) THEN
         CALL med_restart_out ( head_grid, config_flags )
         CALL med_hist_out ( head_grid , HISTORY_ALARM, config_flags )
         CALL wrf_debug ( 0 , ' 0 h restart only wrf: SUCCESS COMPLETE WRF' )
         CALL wrfjedi_finalize(head_grid)
      END IF
   ENDIF  

   head_grid%start_subtime = domain_get_start_time ( head_grid )
   head_grid%stop_subtime = domain_get_stop_time ( head_grid )
   head_grid_start_subtime = domain_get_start_time ( head_grid )
   head_grid_stop_subtime = domain_get_stop_time ( head_grid )

   model_config_rec_tmp = model_config_rec
   wrfjedi_model_config_rec => model_config_rec_tmp

! ------------------------------------------------------------------------------
   END SUBROUTINE wrfjedi_geom_init


   SUBROUTINE set_derived_rconfigs(model_config_rec)

      USE module_configure, only : model_config_rec_type
      USE module_state_description, only : DFI_NODFI

      IMPLICIT NONE

      TYPE(model_config_rec_type),intent(inout) :: model_config_rec

      INTEGER :: i



      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % mp_physics_dfi(i) = model_config_rec % mp_physics(i)
        ENDDO
      END IF


      IF ( model_config_rec % dfi_opt .EQ. DFI_NODFI ) THEN
        DO i = 1, model_config_rec % max_dom
           model_config_rec % bl_pbl_physics_dfi(i) = -1
        ENDDO
      ELSE
        DO i = 1, model_config_rec % max_dom
           model_config_rec % bl_pbl_physics_dfi(i) = model_config_rec % bl_pbl_physics(i)
        ENDDO
      END IF


   END SUBROUTINE set_derived_rconfigs

! ------------------------------------------------------------------------------
end module wrfjedi_geom_mod
