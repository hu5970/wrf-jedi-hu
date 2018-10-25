! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module wrfjedi_trajectories

!use wrfjedi_derived_types

implicit none
private

public :: wrfjedi_trajectory, set_traj, delete_traj
public :: wrfjedi_traj_registry

! ------------------------------------------------------------------------------

!> Fortran derived type to hold the model trajectory
type :: wrfjedi_pool_type
  integer :: nf ! how many varaibles to be analyzed
! need to define this
end type wrfjedi_pool_type

type :: wrfjedi_trajectory
  integer :: nf                                  ! Number of variables in fld
  character(len=22), allocatable  :: fldnames(:) ! Variable identifiers
  type (wrfjedi_pool_type), pointer  :: subFields   !---> state variables (to be analyzed)
  type (wrfjedi_pool_type), pointer  :: auxFields   !---> auxiliary variables, such as pressure, t2m, u10, v10, Tsfc
end type wrfjedi_trajectory

#define LISTED_TYPE wrfjedi_trajectory

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrfjedi_traj_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine set_traj(self,flds)
use wrfjedi_fields_mod
! use wrfjedi_pool_routines
implicit none
type(wrfjedi_trajectory), intent(inout) :: self
type(wrfjedi_field)     , intent(in   ) :: flds

 write(*,*) '===> set_traj(self) in wrfjedi_trajectories.F90'

 self%nf = flds%nf
 allocate(self%fldnames( self%nf ))
 self%fldnames = flds%fldnames

! write(*,*) ' associated(self%subFields) = ', associated(self%subFields) 
! if( associated(self%subFields) ) then
!   call wrfjedi_pool_empty_pool(self % subFields)
!   call wrfjedi_pool_destroy_pool(self % subFields)
!   call wrfjedi_pool_empty_pool(self % auxFields)
!   call wrfjedi_pool_destroy_pool(self % auxFields)
! endif

! call wrfjedi_pool_create_pool(self % subFields,self % nf)
! call wrfjedi_pool_clone_pool(flds % subFields, self % subFields)
! call wrfjedi_pool_create_pool(self % auxFields) !???,self % nf)
! call wrfjedi_pool_clone_pool(flds % subFields, self % auxFields)

 write(*,*) '===> DONE set_traj(self) in wrfjedi_trajectories.F90'

end subroutine set_traj

! ------------------------------------------------------------------------------

subroutine delete_traj(self)
implicit none
type(wrfjedi_trajectory), intent(inout) :: self

 write(*,*) '===> delete_traj(self) in wrfjedi_trajectories.F90'

end subroutine delete_traj

! ------------------------------------------------------------------------------

end module wrfjedi_trajectories
