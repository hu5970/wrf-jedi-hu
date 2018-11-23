! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module wrfjedi_fields_mod

use iso_c_binding 
use config_mod
use datetime_mod
use wrfjedi_geom_mod
use ufo_vars_mod
use wrfjedi_kinds, only : kind_real,StrKIND
use ioda_locs_mod
use ufo_geovals_mod
use wrfjedi_getvaltraj_mod, only: wrfjedi_getvaltraj

use wrfjedi_pool_routines, only : wrfjedi_pool_type, &
                                  wrfjedi_pool_iterator_type, &
                                  wrfjedi_pool_add_field, &
                                  wrfjedi_pool_get_field, &
                                  wrfjedi_pool_get_array
use wrfjedi_pool_routines, only : wrfjedi_pool_destroy_pool
use wrfjedi_pool_routines, only : pool_print_members
use wrfjedi_pool_routines, only : wrfjedi_pool_clone_pool, &
                                  wrfjedi_pool_create_pool

use wrfjedi4da_mod, only : da_make_subpool_wrfjedi,       &
                           da_check_grid_content_wrfjedi, &
                           da_zeros,                      &
                           da_self_mult,da_random,da_fldrms,&
                           da_dot_product,da_operator,da_axpy,da_gpnorm


use wrfjedi_derived_types, only: field0DReal,field0DInteger, &
                                 field1DReal,field1DInteger, &
                                 field2DReal,field2DInteger, &
                                 field3DReal,field3DInteger, &
                                 field4DReal

!use wrfjedi_dmpar
!use wrfjedi_derived_types
!use wrfjedi_framework
!use wrfjedi_kind_types
!use init_atm_core_interface
!use wrfjedi_subdriver
!use atm_core
!use mpas4da_mod
!use mpas2ufo_vars_mod
!use wrfjedi_stream_manager
!use wrfjedi_pool_routines
!use wrfjedi_field_routines
!use wrfjedi_constants

implicit none
private

public :: wrfjedi_field, &
        & create, delete, zeros, random, copy, &
        & self_add, self_schur, self_sub, self_mul, axpy, &
        & dot_prod, add_incr, diff_incr, &
        & read_file, write_file, gpnorm, fldrms, &
        & change_resol, getvalues, getvalues_tl, getvalues_ad, &
        & ug_coord, field_to_ug, field_from_ug, &
        & analytic_IC
public :: wrfjedi_field_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold WRF fields

type :: wrfjedi_field
  type (wrfjedi_geom), pointer :: geom              ! grid and MPI infos
  integer :: nf                                  ! Number of variables in fld
  character(len=MAXVARLEN), allocatable  :: fldnames(:) ! Variable identifiers
  type (wrfjedi_pool_type), pointer  :: subFieldsBk !---> background variables (to be analyzed but not real data space)
  type (wrfjedi_pool_type), pointer  :: subFields   !---> state variables (to be analyzed with real space saved)
  type (wrfjedi_pool_type), pointer  :: auxFields   !---> auxiliary variables, such as pressure, t2m, u10, v10, Tsfc (No real space)
!  type (MPAS_Clock_type), pointer :: clock
  integer :: nf_aux
end type wrfjedi_field

#define LISTED_TYPE wrfjedi_field

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrfjedi_field_registry

integer, parameter :: nf_aux = 12

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------
subroutine create(self, geom, vars)

!    use wrfjedi_kind_types

    implicit none

    type(wrfjedi_field), intent(inout)       :: self
    type(wrfjedi_geom),  intent(in), pointer :: geom
    type(ufo_vars),   intent(in)          :: vars

    integer :: nsize, nfields
    integer :: ierr!, ii

    character(len=22), allocatable  :: fldnames_aux(:)

    ! from the namelist
    self % nf =  vars % nv
    allocate(self % fldnames(self % nf))
    self % fldnames(:) = vars % fldnames(:)
    write(*,*)'self % nf =',self % nf
    write(*,*)'link ::',self % fldnames(:)

    ! link geom
    if (associated(geom)) then
      self % geom => geom
    else
      write(*,*)'wrfjedi_fields: geom not associated'
      call abor1_ftn("wrfjedi_fields: geom not associated")
    end if

!    write(*,*) 'List what we have from WRF grid space'
!    call da_check_grid_content_wrfjedi(self % geom % wrfjedi_head_grid)
!    call abor1_ftn("wrfjedi_fields: debug stop point")

    write(*,*)'-- Create a Pool from list of variable ',self % nf
    call da_make_subpool_wrfjedi(self % geom % wrfjedi_head_grid, self % subFieldsBk,&
                                  self % nf, self % fldnames, nfields)
    if ( self % nf .ne. nfields  ) then
       call abor1_ftn("wrfjedi_fields:create: dimension mismatch ", self % nf, nfields)
    end  if
!    
!    call pool_print_members(self % subFieldsBk, 'subFieldsBk')

    call wrfjedi_pool_create_pool(self % subFields, nfields)
    call wrfjedi_pool_clone_pool(self % subFieldsBk, self % subFields)
!    call pool_print_members(self % subFields, 'subFields')
!    call zeros(self) !-- set zero for self % subFields
!    call pool_print_members(self % subFields, 'subFields')

    !--- TODO: aux test: BJJ  !- get this from json ???
    self % nf_aux = nf_aux
    allocate(fldnames_aux(nf_aux))
    fldnames_aux = [ character(len=22) :: "P_TOP","ZNU","ZNW","XLAT","XLONG","LOWLYR","PHP",&
                                          "MUB","Q2","T2","U10","V10" ]
    write(*,*)'-- Create a sub Pool for auxFields'
    call da_make_subpool_wrfjedi(self % geom % wrfjedi_head_grid, self % auxFields,&
                                  nf_aux, fldnames_aux, nfields)
    deallocate(fldnames_aux)
    if ( nf_aux .ne. nfields  ) then
       call abor1_ftn("wrfjedi_fields:create: dimension mismatch ",nf_aux, nfields)
    end  if
!    call pool_print_members(self % auxFields, 'auxFields')

!    call abor1_ftn("wrfjedi_fields: debug stop point")
end subroutine create

! ------------------------------------------------------------------------------

subroutine delete(self)

   implicit none
   type(wrfjedi_field), intent(inout) :: self
   integer :: ierr = 0 
  
   if (allocated(self % fldnames)) deallocate(self % fldnames)
   if (associated(self % subFieldsBk)) then
      write(*,*)'--> deallocate subFieldsBk Pool'
      call wrfjedi_pool_destroy_pool(self % subFieldsBk)
   end if
   if (associated(self % subFields)) then
      write(*,*)'--> deallocate subFields Pool'
! this one needs set to .true. to release memory allocated by array
!      call wrfjedi_pool_empty_pool(self % subFields)
      call wrfjedi_pool_destroy_pool(self % subFields, .true.)
   end if
   if (associated(self % auxFields)) then
      write(*,*)'--> deallocate auxFields Pool'
!      call wrfjedi_pool_empty_pool(self % auxFields)
      call wrfjedi_pool_destroy_pool(self % auxFields)
   end if
   write(*,*)'--> wrfjedi_fields done deallocate'

   return

end subroutine delete

! ------------------------------------------------------------------------------

subroutine zeros(self)

   implicit none
   type(wrfjedi_field), intent(inout) :: self

   call da_zeros(self % subFields)

end subroutine zeros

! ------------------------------------------------------------------------------

subroutine random(self)

   implicit none
   type(wrfjedi_field), intent(inout) :: self
   
   call da_random(self % subFields)

end subroutine random

! ------------------------------------------------------------------------------

subroutine copy(self,rhs)

   implicit none
   type(wrfjedi_field), intent(inout) :: self
   type(wrfjedi_field), intent(in)    :: rhs
   
   write(*,*)'====> copy of wrfjedi_field'

   !TODO: Do we need to empty/destroy subFields and re-create/clone it ?
   !    : Is "wrfjedi_pool_clone_pool" enough?
   !    : Check this, considering the use of "copy" routine in OOPS.

   ! Duplicate the members of rhs into self and do a deep copy
   ! of the fields from self % subFields to rhs % subFields
!   call wrfjedi_pool_empty_pool(self % subFields)
   call wrfjedi_pool_destroy_pool(self % subFields)
   self % nf = rhs % nf
   call wrfjedi_pool_create_pool(self % subFields,self % nf)
   call wrfjedi_pool_clone_pool(rhs % subFields, self % subFields)

   call wrfjedi_pool_destroy_pool(self % subFieldsBk)
   call wrfjedi_pool_create_pool(self % subFieldsBk,self % nf)
   call wrfjedi_pool_clone_pool(rhs % subFieldsBk, self % subFieldsBk)

!   call wrfjedi_pool_empty_pool(self % auxFields)
   call wrfjedi_pool_destroy_pool(self % auxFields)
   call wrfjedi_pool_create_pool(self % auxFields,nf_aux)
   call wrfjedi_pool_clone_pool(rhs % auxFields, self % auxFields)

   ! We should consider adding a subroutine just updating the fields
   ! call wrfjedi_pool_copy_fied() 
 
   write(*,*)'====> copy of wrfjedi_field done'
  
end subroutine copy

! ------------------------------------------------------------------------------

subroutine self_add(self,rhs)

   implicit none
   type(wrfjedi_field), intent(inout) :: self
   type(wrfjedi_field), intent(in)    :: rhs
   character(len=StrKIND) :: kind_op

   kind_op = 'add'
   call da_operator(trim(kind_op), self % subFields, rhs % subFields)

end subroutine self_add

! ------------------------------------------------------------------------------

subroutine self_schur(self,rhs)

   implicit none
   type(wrfjedi_field), intent(inout) :: self
   type(wrfjedi_field), intent(in)    :: rhs
   character(len=StrKIND) :: kind_op

   kind_op = 'schur'
   call da_operator(trim(kind_op), self % subFields, rhs % subFields)

end subroutine self_schur

! ------------------------------------------------------------------------------

subroutine self_sub(self,rhs)

   implicit none
   type(wrfjedi_field), intent(inout) :: self
   type(wrfjedi_field), intent(in)    :: rhs
   character(len=StrKIND) :: kind_op

   kind_op = 'sub'
   call da_operator(trim(kind_op), self % subFields, rhs % subFields)

end subroutine self_sub

! ------------------------------------------------------------------------------

subroutine self_mul(self,zz)

   implicit none
   type(wrfjedi_field),     intent(inout) :: self
   real(kind=kind_real), intent(in)    :: zz

   call da_self_mult(self % subFields, zz)

end subroutine self_mul

! ------------------------------------------------------------------------------

subroutine axpy(self,zz,rhs)

   implicit none
   type(wrfjedi_field),     intent(inout) :: self
   real(kind=kind_real), intent(in)    :: zz
   type(wrfjedi_field),     intent(in)    :: rhs

   call da_axpy(self % subFields, rhs % subFields, zz)

end subroutine axpy

! ------------------------------------------------------------------------------

subroutine dot_prod(fld1,fld2,zprod)

   implicit none
   type(wrfjedi_field),     intent(in)    :: fld1, fld2
   real(kind=kind_real), intent(inout) :: zprod

!   call da_dot_product(fld1 % subFields, fld2 % subFields, fld1 % geom % domain % dminfo, zprod)
   call da_dot_product(fld1 % subFields, fld2 % subFields, zprod)

end subroutine dot_prod

! ------------------------------------------------------------------------------
!> add increment to state
!!
!! \details **add_incr()** adds "increment" to "state", such as
!!          state (containing analysis) = state (containing guess) + increment
!!          Here, we also update "theta", "rho", and "u" (edge-normal wind), which are
!!          close to MPAS prognostic variable.
!!          While conversion to "theta" and "rho" uses full state variables,
!!          conversion to "u" from cell center winds uses their increment to reduce 
!!          the smoothing effect.
!!
subroutine add_incr(self,rhs)

   implicit none
   type(wrfjedi_field), intent(inout) :: self !< state
   type(wrfjedi_field), intent(in)    :: rhs  !< increment
   character(len=StrKIND) :: kind_op

   type (wrfjedi_pool_type), pointer :: state, diag, mesh
   type (field2DReal), pointer :: field2d_t, field2d_p, field2d_qv, field2d_uRz, field2d_uRm, &
                                  field2d_th, field2d_rho, field2d_u, field2d_u_inc

   integer :: iCount,i

   iCount=0
   if(self%geom%max_dom /= rhs%geom%max_dom) iCount = iCount + 1
   if(iCount == 0) then
      do i=1,self%geom%max_dom
         if(self%geom%e_we(i) /= rhs%geom%e_we(i)) iCount = iCount + 1
         if(self%geom%e_sn(i) /= rhs%geom%e_sn(i)) iCount = iCount + 1
         if(self%geom%e_vert(i) /= rhs%geom%e_vert(i)) iCount = iCount + 1
      enddo
   endif

  ! GD: I don''t see any difference than for self_add other than subFields can contain
  ! different variables than wrfjedi_field and the resolution of incr can be different. 

   if (iCount==0) then
      !NOTE: first, get full state of "subFields" variables
      kind_op = 'add'
      call da_operator(trim(kind_op), self % subFields, rhs % subFields)

     !NOTE: second, also update variables which are closely related to MPAS prognostic vars.
     !  update theta from temperature and pressure
     !  update rho   from temperature, pressure, and index_qv
!      call wrfjedi_pool_get_field(self % subFields,            'temperature', field2d_t)
!      call wrfjedi_pool_get_field(self % subFields,               'pressure', field2d_p)
!      call wrfjedi_pool_get_field(self % subFields,               'index_qv', field2d_qv)
!      call wrfjedi_pool_get_field(self % subFields,      'uReconstructZonal', field2d_uRz)
!      call wrfjedi_pool_get_field(self % subFields, 'uReconstructMeridional', field2d_uRm)
!      call wrfjedi_pool_get_field(self % auxFields,                  'theta', field2d_th)
!      call wrfjedi_pool_get_field(self % auxFields,                    'rho', field2d_rho)
!
!      field2d_th % array(:,:) = field2d_t % array(:,:) * &
!                 ( 100000.0_kind_real / field2d_p % array(:,:) ) ** ( rgas / cp )
!      write(*,*) 'add_inc: theta min/max = ', minval(field2d_th % array), maxval(field2d_th % array)
!      field2d_rho % array(:,:) = field2d_p % array(:,:) /  ( rgas * field2d_t % array(:,:) * &
!                 ( 1.0_kind_real + (rv/rgas - 1.0_kind_real) * field2d_qv % array(:,:) ) )
!      write(*,*) 'add_inc: rho min/max = ', minval(field2d_rho % array), maxval(field2d_rho % array)
!
!     !  update u     from uReconstructZonal and uReconstructMeridional "incrementally"
!      call wrfjedi_pool_get_field(self % auxFields,                      'u', field2d_u)
!      call wrfjedi_pool_get_field( rhs % subFields,      'uReconstructZonal', field2d_uRz)
!      call wrfjedi_pool_get_field( rhs % subFields, 'uReconstructMeridional', field2d_uRm)
!      call wrfjedi_pool_get_field( rhs % auxFields,                      'u', field2d_u_inc)
!      write(*,*) 'add_inc: u_inc min/max = ', minval(field2d_uRz % array), maxval(field2d_uRz % array)
!      write(*,*) 'add_inc: v_inc min/max = ', minval(field2d_uRm % array), maxval(field2d_uRm % array)
!      write(*,*) 'add_inc: u_guess min/max = ', minval(field2d_u % array), maxval(field2d_u % array)
!      write(*,*) 'add_inc: u_inc min/max = ', minval(field2d_u_inc % array), maxval(field2d_u_inc % array)
!      field2d_u % array(:,:) = field2d_u % array(:,:) + field2d_u_inc % array(:,:)
!      write(*,*) 'add_inc: u_analy min/max = ', minval(field2d_u % array), maxval(field2d_u % array)

   else
      call abor1_ftn("wrfjedi_fields:add_incr: dimension mismatch")
   endif

   return

end subroutine add_incr

! ------------------------------------------------------------------------------

subroutine diff_incr(lhs,x1,x2)

   implicit none
   type(wrfjedi_field), intent(inout) :: lhs
   type(wrfjedi_field), intent(in)    :: x1
   type(wrfjedi_field), intent(in)    :: x2
   character(len=StrKIND) :: kind_op

   integer :: iCount,i

   iCount=0
   if(x1%geom%max_dom /= x2%geom%max_dom) iCount = iCount + 1
   if(iCount == 0) then
      do i=1,x1%geom%max_dom
         if(x1%geom%e_we(i) /= x2%geom%e_we(i)) iCount = iCount + 1
         if(x1%geom%e_sn(i) /= x2%geom%e_sn(i)) iCount = iCount + 1
         if(x1%geom%e_vert(i) /= x2%geom%e_vert(i)) iCount = iCount + 1
      enddo
   endif

   call zeros(lhs)
   if (iCount == 0) then
     kind_op = 'sub'
     call da_operator(trim(kind_op), lhs % subFields, x1 % subFields, x2 % subFields)
   else
     call abor1_ftn("wrfjedi_fields:diff_incr: states not at same resolution")
   endif

   return

end subroutine diff_incr

! ------------------------------------------------------------------------------

subroutine change_resol(fld,rhs)

   implicit none
   type(wrfjedi_field), intent(inout) :: fld
   type(wrfjedi_field), intent(in)    :: rhs

   integer :: iCount,i
   iCount=0
   if(fld%geom%max_dom /= rhs%geom%max_dom) iCount = iCount + 1
   if(iCount == 0) then
      do i=1,fld%geom%max_dom
         if(fld%geom%e_we(i) /= rhs%geom%e_we(i)) iCount = iCount + 1
         if(fld%geom%e_sn(i) /= rhs%geom%e_sn(i)) iCount = iCount + 1
         if(fld%geom%e_vert(i) /= rhs%geom%e_vert(i)) iCount = iCount + 1
      enddo
   endif
   ! FIXME: We just copy rhs to fld for now. Need an actual interpolation routine later. (SH)
   if (iCount==0) then
     call copy(fld, rhs)
   else
     write(*,*) fld%geom%e_we(1),fld%geom%e_sn(1),fld%geom%e_vert(1)
     call abor1_ftn("wrfjedi_fields:field_resol: dimension mismatch")
   endif

end subroutine change_resol

! ------------------------------------------------------------------------------
!> Analytic Initialization for the MPAS Model
!!
!! \details **analytic_IC()** initializes the MPAS Field and State objects using one of
!! several alternative idealized analytic models.  This is intended to facilitate testing by
!! eliminating the need to read in the initial state from a file and by providing exact expressions
!! to test interpolations.  This function is activated by setting the "analytic_init" field in the
!! "initial" or "StateFile" section of the configuration file.
!!
!! Initialization options that begin with "dcmip" refer to tests defined by the multi-institutional
!! 2012 [Dynamical Core Intercomparison Project](https://earthsystealcmcog.org/projects/dcmip-2012)
!! and the associated Summer School, sponsored by NOAA, NSF, DOE, NCAR, and the University of Michigan.
!!
!! Currently implemented options for analytic_init include:
!! * invent-state: Backward compatibility with original analytic init option
!! * dcmip-test-1-1: 3D deformational flow
!! * dcmip-test-1-2: 3D Hadley-like meridional circulation
!! * dcmip-test-3-1: Non-hydrostatic gravity wave
!! * dcmip-test-4-0: Baroclinic instability
!!
!! \author J. Guerrette (adapted from fv3jedi code by M. Miesch)
!! \date July, 2018: Created
!!
subroutine analytic_IC(fld, geom, c_conf, vdate)

!  use kinds

!  !MPAS Test Cases
!  !JJG: This initialization requires the init_atmospher_core core_type 
!  !      in the MPAS library for OOPS, but currently it is not included
!  use init_atm_core, only: init_atm_core_run!, init_atm_core_finalize (could be used for cleanup...)

  implicit none

  type(wrfjedi_field), intent(inout)     :: fld !< Fields
  type(wrfjedi_geom), target, intent(in) :: geom    !< Geometry 
  type(c_ptr), intent(in)                :: c_conf   !< Configuration
  type(datetime), intent(inout)          :: vdate    !< DateTime

  ! Pointer to geometry component of field object
  fld%geom => geom

  WRITE(*,*) "wrfjedi_fields:analytic_init: "

   write(*,*)'==> end wrfjedi_fields:analytic_init'

end subroutine analytic_IC



! ------------------------------------------------------------------------------
subroutine invent_state(fld,config)

   use kinds

   implicit none

   type(wrfjedi_field), intent(inout) :: fld    !< Model fields
   type(c_ptr), intent(in)         :: config  !< Configuration structure

   !- read/interp.

   !Diagnostic vars (diag pool)
   !u
   !v
return
end subroutine invent_state
! -----------------------------------------------------------------------------------------------------------

subroutine read_file(fld, c_conf, vdate)

   implicit none
   type(wrfjedi_field), intent(inout) :: fld   !< Fields
   type(c_ptr),      intent(in)    :: c_conf   !< Configuration
   type(datetime),   intent(inout) :: vdate    !< DateTime
   character(len=20) :: sdate
   character(len=80) :: temp_filename
   integer                 :: ierr = 0

!   type (wrfjedi_pool_type), pointer :: state, diag, mesh
!   type (field2DReal), pointer :: field2d, field2d_b, field2d_c

   write(*,*)'==> read fields'
   sdate = config_get_string(c_conf,len(sdate),"date")
   call datetime_set(sdate, vdate)

   write(*,*) '=====>  read sdate ',sdate
   temp_filename = config_get_string(c_conf,len(temp_filename),&
                      "filename")
   write(*,*)'===> Reading ',trim(temp_filename)
   !temp_filename = 'restart.$Y-$M-$D_$h.$m.$s.nc'
   ! GD look at oops/src/util/datetime_mod.F90
   ! we probably need to extract from vdate a string to enforce the reading ..
   ! and then can be like this ....
   ! TODO: we can get streamID from json
   !streamID = 'restart'
   !streamID = 'input'

end subroutine read_file

! ------------------------------------------------------------------------------

subroutine write_file(fld, c_conf, vdate)

use duration_mod
   implicit none
   type(wrfjedi_field), intent(inout) :: fld    !< Fields
   type(c_ptr),      intent(in)    :: c_conf !< Configuration
   type(datetime),   intent(inout) :: vdate  !< DateTime
   character(len=20)       :: validitydate
   integer                 :: ierr
   character (len=StrKIND) ::  temp_filename

   call datetime_to_string(vdate, validitydate)
   write(*,*)'==> write fields at ',trim(validitydate)
   temp_filename = config_get_string(c_conf,len(temp_filename)&
                      ,"filename")
   write(*,*)'==> writing ',trim(temp_filename)

end subroutine write_file

! ------------------------------------------------------------------------------

subroutine gpnorm(fld, nf, pstat)

   implicit none
   type(wrfjedi_field),     intent(in)  :: fld
   integer,              intent(in)  :: nf
   real(kind=kind_real), intent(out) :: pstat(3, nf)

   real(kind=kind_real), allocatable :: pstat_aux(:,:)

   pstat=0.0
!   call da_gpnorm(fld % subFields, fld % geom % domain % dminfo, fld%nf, pstat) 
   call da_gpnorm(fld % subFields, fld%nf, pstat) 
!   call da_gpnorm(fld % subFieldsBk, fld%nf, pstat) 
!   allocate(pstat_aux(3, fld%nf_aux))
!   call da_gpnorm(fld % auxFields, fld%nf_aux, pstat_aux) 
!   deallocate(pstat_aux)

end subroutine gpnorm

! ------------------------------------------------------------------------------

subroutine fldrms(fld, prms)

   implicit none
   type(wrfjedi_field),     intent(in)  :: fld
   real(kind=kind_real), intent(out) :: prms

!   call da_fldrms(fld % subFields, fld % geom % domain % dminfo, prms)
   call da_fldrms(fld % subFields, prms)

end subroutine fldrms

! ------------------------------------------------------------------------------

subroutine getvalues(fld, locs, vars, gom, traj)

!   use type_bump, only: bump_type
!   use mpas2ufo_vars_mod !, only: usgs_to_crtm_mw, wrf_to_crtm_soil

   implicit none
   type(wrfjedi_field),                        intent(in)    :: fld
   type(ioda_locs),                         intent(in)    :: locs
   type(ufo_vars),                          intent(in)    :: vars
   type(ufo_geovals),                       intent(inout) :: gom
   type(wrfjedi_getvaltraj), optional, target, intent(inout) :: traj
   
   character(len=*), parameter :: myname = 'getvalues'

!   type(bump_type), target  :: bump
!   type(bump_type), pointer :: pbump
!   logical,         target  :: bump_alloc
!   logical,         pointer :: pbumpa
   
!   integer :: ii, jj, ji, jvar, jlev, ngrid, nobs, ivar
!   real(kind=kind_real), allocatable :: mod_field(:,:), mod_field_ext(:,:)
!   real(kind=kind_real), allocatable :: obs_field(:,:)
!   real(kind=kind_real), allocatable :: tmp_field(:,:)  !< for wspeed/wdir
   
!   type (wrfjedi_pool_type), pointer :: pool_ufo  !< pool with ufo variables
!   type (wrfjedi_pool_iterator_type) :: poolItr
!   real (kind=kind_real), pointer :: r0d_ptr_a, r0d_ptr_b
!   real (kind=kind_real), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b
!   real (kind=kind_real), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
!   real (kind=kind_real), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b
!   integer, dimension(:), pointer :: i1d_ptr_a, i1d_ptr_b
!   integer, allocatable  :: index_nn(:)
!   real (kind=kind_real), allocatable :: weight_nn(:)
!   type (wrfjedi_pool_type), pointer :: pool_tmp  !< temporary pool for setting trajectory
!   type (field2DReal), pointer :: field2d => null()     !< for setting trajectory
!   type (field2DReal), pointer :: field2d_src => null() !< for setting trajectory
!
!   real(kind=kind_real) :: wdir           !< for wind direction
!   integer :: ivarw, ivarl, ivari, ivars  !< for sfc fraction indices


   ! Get grid dimensions and checks
   ! ------------------------------
   write(*,*) '---- Leaving getvalues ---'
end subroutine getvalues

! ------------------------------------------------------------------------------

subroutine getvalues_tl(fld, locs, vars, gom, traj)

   implicit none
   type(wrfjedi_field),      intent(inout) :: fld
   type(ioda_locs),       intent(in)    :: locs
   type(ufo_vars),        intent(in)    :: vars
   type(ufo_geovals),     intent(inout) :: gom
   type(wrfjedi_getvaltraj), intent(in)    :: traj

!   character(len=*), parameter :: myname = 'getvalues_tl'
!   
!   integer :: ii, jj, ji, jvar, jlev, ngrid, nobs, ivar
!   real(kind=kind_real), allocatable :: mod_field(:,:)
!   real(kind=kind_real), allocatable :: obs_field(:,:)
!   
!   type (wrfjedi_pool_type), pointer :: pool_ufo
!   type (wrfjedi_pool_iterator_type) :: poolItr
!   real (kind=kind_real), pointer :: r0d_ptr_a, r0d_ptr_b
!   real (kind=kind_real), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b
!   real (kind=kind_real), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
!   real (kind=kind_real), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b
   
   ! Check traj is implemented
   ! -------------------------
   write(*,*) '---- Leaving getvalues_tl ---'
end subroutine getvalues_tl

! ------------------------------------------------------------------------------

subroutine getvalues_ad(fld, locs, vars, gom, traj)

   implicit none
   type(wrfjedi_field),      intent(inout) :: fld
   type(ioda_locs),       intent(in)    :: locs
   type(ufo_vars),        intent(in)    :: vars
   type(ufo_geovals),     intent(inout) :: gom
   type(wrfjedi_getvaltraj), intent(in)    :: traj

   character(len=*), parameter :: myname = 'getvalues_ad'

!   integer :: ii, jj, ji, jvar, jlev, ngrid, nobs, ivar
!   real(kind=kind_real), allocatable :: mod_field(:,:)
!   real(kind=kind_real), allocatable :: obs_field(:,:)

!   type (wrfjedi_pool_type), pointer :: pool_ufo
!   type (wrfjedi_pool_iterator_type) :: poolItr
!   real (kind=kind_real), pointer :: r0d_ptr_a, r0d_ptr_b
!   real (kind=kind_real), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b
!   real (kind=kind_real), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
!   real (kind=kind_real), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b

   ! Check traj is implemented
   ! -------------------------
   write(*,*) '---- Leaving getvalues_ad ---' 
end subroutine getvalues_ad

! ------------------------------------------------------------------------------

subroutine initialize_interp(grid, locs, bump)

   use fckit_mpi_module, only: fckit_mpi_comm
   use wrfjedi_geom_mod, only: wrfjedi_geom
   use type_bump, only: bump_type
   
   implicit none
   type(wrfjedi_geom),          intent(in)  :: grid
   type(ioda_locs),          intent(in)  :: locs
   type(bump_type), pointer, intent(out) :: bump
   
!   type(fckit_mpi_comm) :: f_comm
!
!   logical, save :: interp_initialized = .FALSE.
!   
!   integer :: mod_nz,mod_num
!   real(kind=kind_real), allocatable :: mod_lat(:), mod_lon(:) 
!   
!   real(kind=kind_real), allocatable :: area(:),vunit(:,:)
!   logical, allocatable :: lmask(:,:)
!
!   integer, save :: bumpcount = 0
!   character(len=5) :: cbumpcount
!   character(len=17) :: bump_nam_prefix
   
!!   integer :: ii, jj, ji, jvar, jlev
 
!   f_comm = fckit_mpi_comm()

   ! Each bump%nam%prefix must be distinct
   ! -------------------------------------
end subroutine initialize_interp

! ------------------------------------------------------------------------------

subroutine interp_checks(cop, fld, locs, vars, gom)

   implicit none
   character(len=2),  intent(in) :: cop
   type(wrfjedi_field),  intent(in) :: fld
   type(ioda_locs),   intent(in) :: locs
   type(ufo_vars),    intent(in) :: vars
   type(ufo_geovals), intent(in) :: gom
   
   integer :: jvar
   character(len=26) :: cinfo
   
!   cinfo="wrfjedi_fields:checks "//cop//" : "
   
   !Check things are the sizes we expect
   write(*,*)'interp_checks ',cinfo,' done'
   
end subroutine interp_checks

! ------------------------------------------------------------------------------

subroutine ug_size(self, ug)

   use unstructured_grid_mod
   
   implicit none
   type(wrfjedi_field),        intent(in)    :: self
   type(unstructured_grid), intent(inout) :: ug
   integer :: igrid
   
end subroutine ug_size

! ------------------------------------------------------------------------------

subroutine ug_coord(self, ug, colocated)

   use unstructured_grid_mod
   
   implicit none
   type(wrfjedi_field),        intent(in)    :: self
   type(unstructured_grid), intent(inout) :: ug
   integer,                 intent(in)    :: colocated
   
   integer :: jl, igrid
   
   ! Define size

end subroutine ug_coord

! ------------------------------------------------------------------------------

subroutine field_to_ug(self, ug, colocated)

!   use wrfjedi_pool_routines
   use unstructured_grid_mod
   
   implicit none
   type(wrfjedi_field),        intent(in)    :: self
   type(unstructured_grid), intent(inout) :: ug
   integer,                 intent(in)    :: colocated
   
!   integer :: idx_var,jC,jl  
!   type (wrfjedi_pool_iterator_type) :: poolItr
!   real (kind=kind_real), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
!   type(ufo_vars) :: vars ! temporary to access variable "index" easily

   ! Set list of variables

end subroutine field_to_ug

! -----------------------------------------------------------------------------

subroutine field_from_ug(self, ug)

!   use wrfjedi_pool_routines
   use unstructured_grid_mod

   implicit none
   type(wrfjedi_field),        intent(inout) :: self
   type(unstructured_grid), intent(in)    :: ug
   
!   integer :: idx_var,jC,jl
!   type (wrfjedi_pool_iterator_type) :: poolItr
!   real (kind=kind_real), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
!   type(ufo_vars) :: vars ! temporary to access variable "index" easily

   ! TODO: Since only local locations are updated/transferred from ug, 
   !       need MPAS HALO comms before using these fields in MPAS

end subroutine field_from_ug

! ------------------------------------------------------------------------------

end module wrfjedi_fields_mod
