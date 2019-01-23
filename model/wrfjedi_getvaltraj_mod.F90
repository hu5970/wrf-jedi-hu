
!> Fortran module handling interpolation trajectory for the WRF model

module wrfjedi_getvaltraj_mod

!General JEDI uses
use kinds
use iso_c_binding
use type_bump, only: bump_type
!use wrfjedi_pool_routines

implicit none
private

public wrfjedi_getvaltraj
public wrfjedi_getvaltraj_registry
public c_wrfjedi_getvaltraj_setup, c_wrfjedi_getvaltraj_delete

type :: wrfjedi_pool_type
  integer :: nf ! how many varaibles to be analyzed
! need to define this
end type wrfjedi_pool_type

type :: wrfjedi_getvaltraj
 integer :: nobs, ngrid
 type (wrfjedi_pool_type), pointer :: pool_traj
 type(bump_type) :: bump
 integer :: nsize = 2 !< size of pool, currently for temperature, index_qv
 logical :: lalloc = .false.
end type wrfjedi_getvaltraj

#define LISTED_TYPE wrfjedi_getvaltraj

!> Linked list interface - defines registry_t type
#include "linkedList_i.f"

!> Global registry
type(registry_t) :: wrfjedi_getvaltraj_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "linkedList_c.f"

! ------------------------------------------------------------------------------

subroutine c_wrfjedi_getvaltraj_setup(c_key_self) bind(c,name='wrfjedi_getvaltraj_setup_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self
type(wrfjedi_getvaltraj), pointer :: self
integer :: nsize  !< size of trajectory pool

! Init, add and get key
! ---------------------
call wrfjedi_getvaltraj_registry%init()
call wrfjedi_getvaltraj_registry%add(c_key_self)
call wrfjedi_getvaltraj_registry%get(c_key_self,self)

print*, 'dh: getvaltraj_setup', c_key_self

self%lalloc = .false.
self%nobs = 0
self%ngrid = 0

! call wrfjedi_pool_create_pool(self % pool_traj, self % nsize)

end subroutine c_wrfjedi_getvaltraj_setup

! ------------------------------------------------------------------------------

subroutine c_wrfjedi_getvaltraj_delete(c_key_self) bind(c,name='wrfjedi_getvaltraj_delete_f90')

implicit none
integer(c_int), intent(inout) :: c_key_self
type(wrfjedi_getvaltraj), pointer :: self

! Get key
call wrfjedi_getvaltraj_registry%get(c_key_self, self)

if (self%lalloc) then
  self%nobs = 0
  self%ngrid = 0
!  call wrfjedi_pool_empty_pool( self % pool_traj )
!  call wrfjedi_pool_destroy_pool( self % pool_traj )
  call self%bump%dealloc
  self%lalloc = .false.
endif

! Remove key
call wrfjedi_getvaltraj_registry%remove(c_key_self)

end subroutine c_wrfjedi_getvaltraj_delete

! ------------------------------------------------------------------------------

end module wrfjedi_getvaltraj_mod
