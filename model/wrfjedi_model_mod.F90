! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module wrfjedi_model_mod

use iso_c_binding
use wrfjedi_geom_mod
USE module_domain_type, only: domain
USE module_configure, only : model_config_rec_type

use wrfjedi_fields_mod
use wrfjedi_trajectories

use datetime_mod
!use wrfjedi_derived_types
!use wrfjedi_framework
!use wrfjedi_kind_types
!use wrfjedi_subdriver
!use atm_core
!use wrfjedi_stream_manager
!use mpas4da_mod
use wrfjedi_kinds, only : kind_real
!use wrfjedi_constants, only : rgas, cp

implicit none
private
public :: wrfjedi_model, & 
        & model_setup, model_delete, &
        & model_prepare_integration, model_prepare_integration_tl, model_prepare_integration_ad, &
        & model_propagate, model_propagate_tl, model_propagate_ad, &
        & model_prop_traj, model_wipe_traj, &
        & wrfjedi_model_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold model definition
type :: wrfjedi_model
   ! GD: for now, model is just using the full geom structure
   ! we update the fields for time integration using subfield
   ! from wrfjedi_field
   type (domain), pointer :: wrfjedi_head_grid => null()
   type (model_config_rec_type), pointer :: wrfjedi_model_config_rec => null()
   real (kind=kind_real) :: dt
end type wrfjedi_model

#define LISTED_TYPE wrfjedi_model

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrfjedi_model_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine model_setup(self, geom, c_conf)

   USE module_configure, only : model_config_rec

   implicit none
   type(c_ptr), intent(in) :: c_conf !< pointer to object of class Config
   !type(wrfjedi_model), target :: model  ! should I put intent on these?
   type(wrfjedi_model), intent(inout) :: self  ! should I put intent on these?
   type(wrfjedi_geom)        :: geom

!   character(len=20) :: ststep
!   type(duration) :: dtstep

!   real (kind=kind_real), pointer :: config_dt
!   character (len=StrKIND), pointer :: config_start_time
!   character (len=StrKIND), pointer :: config_restart_timestamp_name
!   character (len=StrKIND), pointer :: config_run_duration
!   character (len=StrKIND), pointer :: config_stop_time
!   character (len=StrKIND) :: startTimeStamp
   
   write(*,*) "---- Inside of Sub. model_setup ----"
#define ModelWRFJEDI_setup
#ifdef ModelWRFJEDI_setup
   self % wrfjedi_head_grid => geom % wrfjedi_head_grid
   self % wrfjedi_model_config_rec => geom % wrfjedi_model_config_rec
   self % dt = model_config_rec% dt(1)

   ! GD:  we need to update some parameters here regarding the json namelist file of oops.
   ! Also, we can add new DA parameters in the MPAS configs file if needed.
!   write(*,*)'config_dt: ',config_dt
!!   write(*,*)'config_start_time: ',trim(config_start_time)
!   write(*,*)'config_restart_timestamp_name: ',trim(config_restart_timestamp_name)
!   write(*,*)'config_run_duration: ',trim(config_run_duration)
!   write(*,*)'config_stop_time: ',trim(config_stop_time)
   write(*,*)'geom % nx: ',geom % e_we(1)
   write(*,*)'geom % ny: ',geom % e_sn(1)
   write(*,*)'geom % nz: ',geom % e_vert(1)
   write(*,*)'ststep: ', self % dt



   ! GD: needs a converter from oops json file format to mpas if the json file drives MPAS
   ! otherwise mpas namelist file can be used.
!   ststep = config_get_string(c_conf,len(ststep),"tstep")
#endif

end subroutine model_setup

! ------------------------------------------------------------------------------

subroutine model_delete(self)

   implicit none
   type(wrfjedi_model) :: self

   INTERFACE
     SUBROUTINE wrfjedi_finalize(wrfjedi_head_grid)
        USE module_domain_type,   only : domain
        TYPE(domain), pointer :: wrfjedi_head_grid
     END SUBROUTINE wrfjedi_finalize
   END INTERFACE


   write(*,*)'===> wrfjedi_finalize'
   call wrfjedi_finalize(self%wrfjedi_head_grid)
   ! For now, all the structure is hold by geom
   write(*,*)'===> model_delete done'

end subroutine model_delete

! ------------------------------------------------------------------------------

subroutine model_prepare_integration(self, flds)

   implicit none

   type(wrfjedi_model) :: self
   type(wrfjedi_field) :: flds
   logical, pointer :: config_do_restart, config_do_DAcycling, config_dt
   integer :: ierr = 0
   real (kind=kind_real), pointer :: dt

!   type (block_type), pointer :: block
!
!   character(len=StrKIND) :: startTimeStamp, stopTimeStamp
!
!   type (wrfjedi_pool_type), pointer :: state
!   type (wrfjedi_pool_type), pointer :: mesh
!   type (wrfjedi_pool_type), pointer :: diag
!   type (field2DReal), pointer :: u_field, pv_edge_field, ru_field, rw_field
!   type (field2DReal), pointer :: uReconstructZonal, uReconstructMeridional
!   character (len=StrKIND), pointer :: xtime
!   character (len=StrKIND), pointer :: config_run_duration
!   type (MPAS_Time_Type) :: startTime, stopTime
!   type (MPAS_Timeinterval_Type) ::  runDuration

   write(*,*)'===> model_prepare_integration'

!   call wrfjedi_run(self%wrfjedi_head_grid)
!   call wrfjedi_finalize(self%wrfjedi_head_grid)

   ! GD: the present design relies on the hypothesis that we run
   ! the model member sequentially using the geom structure
   ! In the reverse case, we will need to create locally domain and 
   ! corelist by calling first the wrfjedi_subdriver for example 
   ! like it is done in geom

   ! here or where increment is computed
!MHU figure out this later, here to setup analysis field and increment
!    call da_copy_sub2all_fields(self % domain, flds % subFields)   
!    call da_copy_sub2all_fields(self % domain, flds % auxFields)   

!#define ModelMPAS_prepare
#ifdef ModelWRFJEDI_prepare
   !-------------------------------------------------------------------
   ! WIND processing U and V resconstruct to the edges
   ! not parallel yet, routine initially from DART
   !-------------------------------------------------------------------

   ! here or where increment is computed
   ! update domain % clock using wrfjedi_field clock and config files

!   startTime = wrfjedi_get_clock_time(flds % clock, MPAS_START_TIME, ierr)
!   stopTime = startTime + runDuration
!   write(*,*)'START_TIME, STOP_TIME: ',trim(startTimeStamp),trim(stopTimeStamp)

! MHU: seems we need to figure out he field for analysis here.

   ! Computation of theta_m from theta and qv
   ! Computation of rho_zz from rho / zz from updated rho
   ! Recoupling of all the variables
!   call wrfjedi_pool_get_config(self % domain % blocklist % configs, 'config_do_restart', config_do_restart)
!   call wrfjedi_pool_get_config(self % domain % blocklist % configs, 'config_do_DAcycling', config_do_DAcycling)
!   call wrfjedi_pool_get_config(self % domain % blocklist % configs, 'config_dt', dt)
!   config_do_restart = .True.
!   config_do_DAcycling = .True.

!   call wrfjedi_pool_get_subpool(self % domain % blocklist % structs, 'state', state)

!   block => self % domain % blocklist
!   do while (associated(block))
!      call wrfjedi_pool_get_subpool(block % structs, 'mesh', mesh)
!      call wrfjedi_pool_get_subpool(block % structs, 'state', state)
!      ! GD: if we do cycling in atm_wrfjedi_init_block we propably need to avoid recomputing wind to the 
!      ! mass center. (avoiding doing twice ... adding a flag). OK for now, probably same problem with dart 
!      call atm_wrfjedi_init_block(self % domain % dminfo, self % domain % streamManager, block, mesh, self % dt)
!      call wrfjedi_pool_get_array(state, 'xtime', xtime, 1)
!      xtime = startTimeStamp
!      block => block % next
!   end do
!   call wrfjedi_dmpar_exch_halo_field(rw_field)
#endif

end subroutine model_prepare_integration

! ------------------------------------------------------------------------------

subroutine model_prepare_integration_ad(self, flds)

   implicit none
   type(wrfjedi_model) :: self
   type(wrfjedi_field) :: flds

   write(*,*)'===> model_prepare_integration_ad'

end subroutine model_prepare_integration_ad

! ------------------------------------------------------------------------------

subroutine model_prepare_integration_tl(self, flds)

   implicit none
   type(wrfjedi_model) :: self
   type(wrfjedi_field) :: flds

   write(*,*)'===> model_prepare_integration_tl'

end subroutine model_prepare_integration_tl

! ------------------------------------------------------------------------------

subroutine model_propagate(self, flds, vdate, vdate_next)

   USE module_domain_type,only: domain
   USE module_domain,only: domain_clock_get,domain_clockprint,&
                           domain_get_current_time,domain_get_time_step,&
                           domain_get_stop_time, &
                           WRFU_TimeInterval,WRFU_Time,WRFU_TimeSet, &
                           WRFU_SUCCESS
   USE module_domain, only: head_grid_current_time,domain_get_current_time
   USE module_domain, only: find_grid_by_id
!   USE module_wrf_error, only: wrf_check_error 
                           
   USE wrfjedi_module_integrate,only: wrfjedi_integrate

   implicit none
   type(wrfjedi_model) :: self
   type(wrfjedi_field) :: flds
   type(datetime)      :: vdate
   type(datetime)      :: vdate_next

   type(domain), pointer :: head_grid
   TYPE(WRFU_TimeInterval) :: lcl_time_step
   TYPE(WRFU_Time) :: lcl_currtime,lcl_stoptime
   character(len=20) :: vdatestring
   character(len=20) :: vdate_next_string

   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second
   INTEGER :: rc

   integer :: ierr = 0

   call datetime_to_string(vdate, vdatestring)
   call datetime_to_string(vdate_next, vdate_next_string)
   write(*,*)'===> model_propagate, valid time: ', vdatestring
   write(*,*)'===> model_propagate, step time : ', vdate_next_string

!   lcl_time_step = domain_get_time_step(self%wrfjedi_head_grid)
!   lcl_currtime  = domain_get_current_time(self%wrfjedi_head_grid)
!   lcl_stoptime  = lcl_currtime
!   lcl_stoptime%basetime%S  = lcl_currtime%basetime%S +  &
!                              lcl_time_step%basetime%S

   read(vdatestring,'(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)') &
                          start_year,start_month,start_day, &
                          start_hour,start_minute,start_second
   CALL WRFU_TimeSet(lcl_currtime, YY=start_year, MM=start_month, DD=start_day, &
                                H=start_hour, M=start_minute, S=start_second,&
                                rc=rc)
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                            'WRFU_TimeSet(lcl_currtime) FAILED', &
                            "model_propagate" , &
                            102  )

   read(vdate_next_string,'(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)') &
                          end_year,end_month,end_day, &
                          end_hour,end_minute,end_second
   CALL WRFU_TimeSet(lcl_stoptime, YY=end_year, MM=end_month, DD=end_day, &
                            H=end_hour, M=end_minute, S=end_second,&
                            rc=rc )
   CALL wrf_check_error( WRFU_SUCCESS, rc, &
                        'WRFU_TimeSet(lcl_stoptime) FAILED', &
                        "model_propagate" , &
                        271  )

   self%wrfjedi_head_grid%start_subtime= lcl_currtime
   self%wrfjedi_head_grid%stop_subtime = lcl_stoptime

   head_grid_current_time=domain_get_current_time(self%wrfjedi_head_grid)

   call domain_clockprint(0,self%wrfjedi_head_grid,'wrfjedi_integrate time:')

   CALL wrfjedi_integrate ( 1,self%wrfjedi_head_grid )

end subroutine model_propagate

! ------------------------------------------------------------------------------

subroutine model_propagate_ad(self, flds, traj)

   implicit none

   type(wrfjedi_model)      :: self
   type(wrfjedi_field)      :: flds
   type(wrfjedi_trajectory) :: traj

   write(*,*)'===> model_prepare_integration_ad'

end subroutine model_propagate_ad

! ------------------------------------------------------------------------------

subroutine model_propagate_tl(self, flds, traj)

   implicit none
   type(wrfjedi_model)      :: self
   type(wrfjedi_field)      :: flds
   type(wrfjedi_trajectory) :: traj

   write(*,*)'===> model_propagate_tl'

end subroutine model_propagate_tl

! ------------------------------------------------------------------------------

subroutine model_prop_traj(self, flds, traj)

   implicit none
   type(wrfjedi_model)      :: self
   type(wrfjedi_field)      :: flds
   type(wrfjedi_trajectory) :: traj

   write(*,*)'===> model_prop_traj in wrfjedi_model_mod.F90'
   call set_traj(traj,flds)

end subroutine model_prop_traj

! ------------------------------------------------------------------------------

subroutine model_wipe_traj(traj)

   implicit none
   type(wrfjedi_trajectory) :: traj

   write(*,*)'===> model_wipe_traj'

end subroutine model_wipe_traj

! ------------------------------------------------------------------------------

end module wrfjedi_model_mod
