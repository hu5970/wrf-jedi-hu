!***********************************************************************
!
!  wrfjedi_pool_routines
!
!> \brief   WRFJEDI Pool Routines
!> \author  Ming Hu
!> \date    04/02/13
!> \details 
!> This module defines subroutines and functions for handling pools.
!
!-----------------------------------------------------------------------
module wrfjedi_pool_routines

   use wrfjedi_kinds,      only : StrKIND,RKIND
   use module_domain_type, only : fieldlist
   use wrfjedi_derived_types, only : field0DReal,field0DInteger, &
                                     field1DReal,field1DInteger, &
                                     field2DReal,field2DInteger, &
                                     field3DReal,field3DInteger, &
                                     field4DReal,field4DInteger
!   use wrfjedi_io_units
!   use wrfjedi_field_routines
!   use wrfjedi_threading
!   use wrfjedi_abort, only : wrfjedi_dmpar_global_abort
   implicit none
   private

   public :: wrfjedi_pool_type, &
             wrfjedi_pool_iterator_type, &
             wrfjedi_pool_get_gridfield, &
             wrfjedi_pool_add_field, &
             wrfjedi_pool_get_field, &
             wrfjedi_pool_get_array
  
   public :: wrfjedi_pool_create_pool, &
             wrfjedi_pool_clone_pool, &
             wrfjedi_pool_empty_pool, &
             wrfjedi_pool_destroy_pool, &
             pool_print_members

   public :: WRFJEDI_POOL_FIELD, &
             WRFJEDI_POOL_REAL,  &
             WRFJEDI_POOL_INTEGER, &
             WRFJEDI_POOL_LOGICAL, &
             WRFJEDI_POOL_CHARACTER

   public ::  wrfjedi_pool_get_next_member, &
              wrfjedi_pool_begin_iteration
!
   public ::  wrfjedi_duplicate_fieldlist
!
!
   integer, parameter :: WRFJEDI_POOL_TABLE_SIZE = 128
!
   integer, parameter :: WRFJEDI_POOL_SILENT = 1001, &
                         WRFJEDI_POOL_WARN   = 1002, &
                         WRFJEDI_POOL_FATAL  = 1003
!
   integer, parameter :: WRFJEDI_POOL_FIELD     = 1004, &
                         WRFJEDI_POOL_CONFIG    = 1005, &
                         WRFJEDI_POOL_DIMENSION = 1006, &
                         WRFJEDI_POOL_SUBPOOL   = 1007, &
                         WRFJEDI_POOL_PACKAGE   = 1008
!
   CHARACTER*1,parameter :: WRFJEDI_POOL_REAL = 'r',    &
                            WRFJEDI_POOL_INTEGER = 'i', &
                            WRFJEDI_POOL_LOGICAL = 'l', &
                            WRFJEDI_POOL_CHARACTER='c'
!   integer, parameter :: WRFJEDI_POOL_REAL      = 1009, &
!                         WRFJEDI_POOL_INTEGER   = 1010, &
!                         WRFJEDI_POOL_LOGICAL   = 1011, &
!                         WRFJEDI_POOL_CHARACTER = 1012
!

   type wrfjedi_pool_member_type
      character (len=StrKIND) :: key
      integer :: keyLen
      integer :: contentsType
      type (fieldlist), pointer :: data => null()
      type (fieldlist), pointer :: bkdata => null()
      type (wrfjedi_pool_member_type), pointer :: next => null()
      type (wrfjedi_pool_member_type), pointer :: iteration_next => null()
      type (wrfjedi_pool_member_type), pointer :: iteration_prev => null()
   end type wrfjedi_pool_member_type

   type wrfjedi_pool_head_type
      type (wrfjedi_pool_member_type), pointer :: head => null()
   end type wrfjedi_pool_head_type

   type wrfjedi_pool_type
      integer :: size
      type (wrfjedi_pool_head_type), dimension(:), pointer :: table => null()
      type (wrfjedi_pool_member_type), pointer :: iterator => null()
      type (wrfjedi_pool_member_type), pointer :: iteration_head => null()
      type (wrfjedi_pool_member_type), pointer :: iteration_tail => null()
   end type wrfjedi_pool_type

   type wrfjedi_pool_iterator_type
      character (len=StrKIND) :: memberName
      integer :: memberType
      character*1 :: dataType
      integer :: nDims
      integer :: nTimeLevels
   end type wrfjedi_pool_iterator_type

   type wrfjedi_pool_field_info_type
      integer :: fieldType
      integer :: nDims
      integer :: nTimeLevels
      logical :: isActive
   end type wrfjedi_pool_field_info_type

   interface wrfjedi_pool_get_gridfield
      module procedure wrfjedi_pool_get_gridfield_0d_real
      module procedure wrfjedi_pool_get_gridfield_1d_real
      module procedure wrfjedi_pool_get_gridfield_2d_real
      module procedure wrfjedi_pool_get_gridfield_3d_real
      module procedure wrfjedi_pool_get_gridfield_4d_real
      module procedure wrfjedi_pool_get_gridfield_0d_int
      module procedure wrfjedi_pool_get_gridfield_1d_int
      module procedure wrfjedi_pool_get_gridfield_2d_int
      module procedure wrfjedi_pool_get_gridfield_3d_int
      module procedure wrfjedi_pool_get_gridfield_4d_int
   end interface wrfjedi_pool_get_gridfield
   
   interface wrfjedi_pool_add_field
      module procedure wrfjedi_pool_add_field_0d_real
      module procedure wrfjedi_pool_add_field_1d_real
      module procedure wrfjedi_pool_add_field_2d_real
      module procedure wrfjedi_pool_add_field_3d_real
      module procedure wrfjedi_pool_add_field_4d_real
!      module procedure wrfjedi_pool_add_field_5d_real
      module procedure wrfjedi_pool_add_field_0d_int
      module procedure wrfjedi_pool_add_field_1d_int
      module procedure wrfjedi_pool_add_field_2d_int
      module procedure wrfjedi_pool_add_field_3d_int
!      module procedure wrfjedi_pool_add_field_0d_char
!      module procedure wrfjedi_pool_add_field_1d_char
!      module procedure wrfjedi_pool_add_field_0d_reals
!      module procedure wrfjedi_pool_add_field_1d_reals
!      module procedure wrfjedi_pool_add_field_2d_reals
!      module procedure wrfjedi_pool_add_field_3d_reals
!      module procedure wrfjedi_pool_add_field_4d_reals
!      module procedure wrfjedi_pool_add_field_5d_reals
!      module procedure wrfjedi_pool_add_field_0d_ints
!!      module procedure wrfjedi_pool_add_field_1d_ints
!      module procedure wrfjedi_pool_add_field_2d_ints
!      module procedure wrfjedi_pool_add_field_3d_ints
!      module procedure wrfjedi_pool_add_field_0d_chars
!      module procedure wrfjedi_pool_add_field_1d_chars
   end interface

   interface wrfjedi_pool_get_field
      module procedure wrfjedi_pool_get_field_0d_real
      module procedure wrfjedi_pool_get_field_1d_real
      module procedure wrfjedi_pool_get_field_2d_real
      module procedure wrfjedi_pool_get_field_3d_real
      module procedure wrfjedi_pool_get_field_4d_real
!      module procedure wrfjedi_pool_get_field_5d_real
      module procedure wrfjedi_pool_get_field_0d_int
      module procedure wrfjedi_pool_get_field_1d_int
      module procedure wrfjedi_pool_get_field_2d_int
      module procedure wrfjedi_pool_get_field_3d_int
!      module procedure wrfjedi_pool_get_field_0d_char
!      module procedure wrfjedi_pool_get_field_1d_char
   end interface

   interface wrfjedi_pool_get_array
      module procedure wrfjedi_pool_get_array_0d_real
      module procedure wrfjedi_pool_get_array_1d_real
      module procedure wrfjedi_pool_get_array_2d_real
      module procedure wrfjedi_pool_get_array_3d_real
      module procedure wrfjedi_pool_get_array_4d_real
!      module procedure wrfjedi_pool_get_array_5d_real
      module procedure wrfjedi_pool_get_array_0d_int
      module procedure wrfjedi_pool_get_array_1d_int
      module procedure wrfjedi_pool_get_array_2d_int
      module procedure wrfjedi_pool_get_array_3d_int
!      module procedure wrfjedi_pool_get_array_0d_char
!      module procedure wrfjedi_pool_get_array_1d_char
   end interface
!
   interface wrfjedi_pool_add_config
!      module procedure wrfjedi_pool_add_config_real
!      module procedure wrfjedi_pool_add_config_int
!      module procedure wrfjedi_pool_add_config_char
!      module procedure wrfjedi_pool_add_config_logical
   end interface
!
   interface wrfjedi_pool_get_config
!      module procedure wrfjedi_pool_get_config_real
!      module procedure wrfjedi_pool_get_config_int
!      module procedure wrfjedi_pool_get_config_char
!      module procedure wrfjedi_pool_get_config_logical
   end interface
!
   interface wrfjedi_pool_add_dimension
!      module procedure wrfjedi_pool_add_dimension_0d
!      module procedure wrfjedi_pool_add_dimension_1d
   end interface
!
   interface wrfjedi_pool_get_dimension
!      module procedure wrfjedi_pool_get_dimension_0d
!      module procedure wrfjedi_pool_get_dimension_1d
   end interface
!
   integer :: currentErrorLevel = WRFJEDI_POOL_SILENT
   integer :: stderrUnit = 6
!
   contains


!-----------------------------------------------------------------------
!  routine wrfjedi_pool_set_error_level
!
!> \brief WRFJEDI Pool Error level set routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!>  This routine sets the internal error level for pools.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_set_error_level(newErrorLevel) !{{{

      implicit none

      integer, intent(in) :: newErrorLevel
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         currentErrorLevel = newErrorLevel
      end if

   end subroutine wrfjedi_pool_set_error_level !}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_error_level
!
!> \brief WRFJEDI Pool Error level get function
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!>  This routine returns the internal error level for pools.
!
!-----------------------------------------------------------------------
!   integer function wrfjedi_pool_get_error_level() !{{{
!
!      implicit none
!
!      wrfjedi_pool_get_error_level = currentErrorLevel
!
!   end function wrfjedi_pool_get_error_level !}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_create_pool
!
!> \brief WRFJEDI Pool creation routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!>  This routine will create a new empty pool and associate newPool to this new
!>  pool location.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_create_pool(newPool, poolSize)!{{{

      implicit none

      type (wrfjedi_pool_type), pointer :: newPool
      integer, intent(in), optional :: poolSize
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()
      
      if ( threadNum == 0 ) then
         allocate(newPool)

         if (present(poolSize)) then
            newPool % size = poolSize
         else
            newPool % size = WRFJEDI_POOL_TABLE_SIZE
         end if
         allocate(newPool % table(newPool % size))
      end if

   end subroutine wrfjedi_pool_create_pool!}}}


!-----------------------------------------------------------------------
!  routine wrfjedi_pool_destroy_pool
!
!> \brief WRFJEDI Pool deallocation routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!>  This routine will destroy a pool associated with inPool.
!
!-----------------------------------------------------------------------
   recursive subroutine wrfjedi_pool_destroy_pool(inPool,releasemem)!{{{

      implicit none

      type (wrfjedi_pool_type), pointer :: inPool
      logical,intent(in),optional :: releasemem

      integer :: i, j
      type (wrfjedi_pool_member_type), pointer :: ptr
      type (fieldlist), pointer :: dptr
      integer :: local_err, threadNum
      logical :: ifreleasemem

      threadNum = wrfjedi_threading_get_thread_num()
      ifreleasemem=.false.
      if(present(releasemem)) then
        if(releasemem) ifreleasemem=.true.
      endif

      if ( threadNum == 0 ) then
         do i=1,inPool % size
   
            ptr => inPool % table(i) % head
            do while(associated(inPool % table(i) % head))
               ptr => inPool % table(i) % head
               inPool % table(i) % head => inPool % table(i) % head % next
               write(*,*) 'wrfjedi_pool_destroy_pool ==>',trim(ptr % data % VarName),ifreleasemem
   
               if (ptr % contentsType == WRFJEDI_POOL_DIMENSION) then
   
               else if (ptr % contentsType == WRFJEDI_POOL_CONFIG) then
   
               else if (ptr % contentsType == WRFJEDI_POOL_FIELD) then
   
                  dptr => ptr % data
   
!                 write(*,*)  trim(dptr%VarName),dptr%Type,dptr%Ndim
                  if(dptr%Type==WRFJEDI_POOL_REAL) then
                     if(dptr%Ndim == 0) then
                        if(ifreleasemem) deallocate(dptr % rfield_0d, stat=local_err)
                        nullify(dptr % rfield_0d)
                     elseif(dptr%Ndim == 1) then
                        if(ifreleasemem) deallocate(dptr % rfield_1d, stat=local_err)
                        nullify(dptr % rfield_1d)
                     else if(dptr%Ndim == 2) then
                        if(ifreleasemem) deallocate(dptr % rfield_2d, stat=local_err)
                        nullify(dptr % rfield_2d)
                     else if(dptr%Ndim == 3) then
                        if(ifreleasemem) deallocate(dptr % rfield_3d, stat=local_err)
                        nullify(dptr % rfield_3d)
                     else if(dptr%Ndim == 4) then
                        if(ifreleasemem) deallocate(dptr % rfield_4d, stat=local_err)
                        nullify(dptr % rfield_4d)
                     else
                        call pool_mesg('While destroying pool, member '//trim(ptr % key)//' has no valid dimension.')
                     endif
                  elseif(dptr%Type==WRFJEDI_POOL_INTEGER) then
                     if(dptr%Ndim == 0) then
                        if(ifreleasemem) deallocate(dptr % ifield_0d, stat=local_err)
                        nullify(dptr % ifield_0d)
                     elseif(dptr%Ndim == 1) then
                        if(ifreleasemem) deallocate(dptr % ifield_1d, stat=local_err)
                        nullify(dptr % ifield_1d)
                     else if(dptr%Ndim == 2) then
                        if(ifreleasemem) deallocate(dptr % ifield_2d, stat=local_err)
                        nullify(dptr % ifield_2d)
                     else if(dptr%Ndim == 3) then
                        if(ifreleasemem) deallocate(dptr % ifield_3d, stat=local_err)
                        nullify(dptr % ifield_3d)
                     else
                        call pool_mesg('While destroying pool, member '//trim(ptr % key)//' has no valid dimension.')
                     endif
                  else
                     call pool_mesg('While destroying pool, member '//trim(ptr % key)//' has no valid variable type.')
                  endif
   
               else if (ptr % contentsType == WRFJEDI_POOL_SUBPOOL) then
   
!                  call wrfjedi_pool_destroy_pool(ptr % data % p)
   
               end if
               deallocate(ptr % data, stat=local_err)
               deallocate(ptr, stat=local_err)
            end do
   
         end do
   
         deallocate(inPool % table, stat=local_err)
         deallocate(inPool, stat=local_err)
      end if

   end subroutine wrfjedi_pool_destroy_pool!}}}


!-----------------------------------------------------------------------
!  routine wrfjedi_pool_empty_pool
!
!> \brief WRFJEDI Pool empty routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!>  This routine will remove all memebers from within a pool associated with inPool.
!
!-----------------------------------------------------------------------
   recursive subroutine wrfjedi_pool_empty_pool(inPool)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool

      integer :: i
      type (wrfjedi_pool_member_type), pointer :: ptr
      integer :: local_err, threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         do i=1,inPool % size

            ptr => inPool % table(i) % head
            do while(associated(inPool % table(i) % head))
               ptr => inPool % table(i) % head
               inPool % table(i) % head => inPool % table(i) % head % next
               if (ptr % contentsType == WRFJEDI_POOL_DIMENSION) then
               else if (ptr % contentsType == WRFJEDI_POOL_CONFIG) then
               else if (ptr % contentsType == WRFJEDI_POOL_PACKAGE) then
               else if (ptr % contentsType == WRFJEDI_POOL_SUBPOOL) then
               end if
               deallocate(ptr, stat=local_err)
            end do

         end do

         nullify(inPool % iterator)
      end if

   end subroutine wrfjedi_pool_empty_pool!}}}


!-----------------------------------------------------------------------
!  routine wrfjedi_pool_clone_pool
!
!> \brief WRFJEDI Pool clone routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This routine assumes destPool is an empty pool. It will clone all of the members
!> from srcPool into destPool.
!
!-----------------------------------------------------------------------
   recursive subroutine wrfjedi_pool_clone_pool(srcPool, destPool, overrideTimeLevels)!{{{

      implicit none

      type (wrfjedi_pool_type), pointer :: srcPool
      type (wrfjedi_pool_type), pointer :: destPool
      integer, intent(in), optional :: overrideTimeLevels


      integer :: i, j, newTimeLevels, minTimeLevels, threadNum
      type (wrfjedi_pool_member_type), pointer :: ptr
      type (fieldlist), pointer :: dptr
      type (wrfjedi_pool_member_type), pointer :: newmem

      threadNum = wrfjedi_threading_get_thread_num()
      newTimeLevels = -1

      if (present(overrideTimeLevels)) then
         newTimeLevels = overrideTimeLevels

         if (newTimeLevels < 1) then
            call wrfjedi_pool_set_error_level(WRFJEDI_POOL_FATAL)
            call pool_mesg('ERROR in wrfjedi_pool_clone_pool: Input time levels cannot be less than 1.')
         end if
      end if

!TODO: Make use of overrideTimeLevels. This routine needs to create a new set of time levels.
!TODO: should we force destPool to have the same table size as srcPool?

      !TODO: Allow threading on copy calls below.
      if ( threadNum == 0 ) then
         ptr => srcPool % iteration_head
         do while(associated(ptr))

            allocate(newmem)
            newmem % key = ptr % key
            newmem % keyLen = ptr % keyLen
            newmem % contentsType = ptr % contentsType
            allocate(newmem % data)
            newmem % data % Type = ptr % data % Type
            newmem % data % Ndim = ptr % data % Ndim

            if (ptr % contentsType == WRFJEDI_POOL_DIMENSION) then

            else if (ptr % contentsType == WRFJEDI_POOL_CONFIG) then

            else if (ptr % contentsType == WRFJEDI_POOL_FIELD) then

               dptr => ptr % data
               call wrfjedi_duplicate_fieldlist(dptr, newmem % data)

            else if (ptr % contentsType == WRFJEDI_POOL_SUBPOOL) then
!
!                call wrfjedi_pool_create_pool(newmem % data % p, poolSize = ptr % data % p % size)
!                call wrfjedi_pool_clone_pool(ptr % data % p, newmem % data % p)
!
            end if
!
!            write(*,*) 'wrfjedi_pool_clone_pool=',trim(newmem % data % VarName)
            if (.not. pool_add_member(destPool, newmem % key, newmem)) then
               call pool_mesg('Error: Had problems adding '//trim(newmem % key)//' to clone of pool.')
            end if

            ptr => ptr % iteration_next
         end do
      end if

   end subroutine wrfjedi_pool_clone_pool!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_copy_pool
!!
!!> \brief WRFJEDI Pool copy routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine assumes srcPool and destPool have identical members. It will
!!> copy the data from the members of srcPool into the members of destPool.
!!
!!-----------------------------------------------------------------------
!   recursive subroutine wrfjedi_pool_copy_pool(srcPool, destPool)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), pointer :: srcPool
!      type (wrfjedi_pool_type), pointer :: destPool
!
!
!      integer :: i, j, threadNum
!      type (wrfjedi_pool_member_type), pointer :: ptr
!      type (fieldlist), pointer :: dptr
!      type (fieldlist), pointer :: mem
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         do i=1,srcPool % size
!
!            ptr => srcPool % table(i) % head
!            do while(associated(ptr))
!
!               if (ptr % contentsType == WRFJEDI_POOL_DIMENSION) then
!
!                  mem => pool_get_member(destPool, ptr % key, WRFJEDI_POOL_DIMENSION)
!                  if (.not. associated(mem)) then
!                     call wrfjedi_pool_set_error_level(WRFJEDI_POOL_FATAL)
!                     call pool_mesg('ERROR: Destination pool does not contain member '//trim(ptr % key)//'.')
!                  end if
!                  if (ptr % data % contentsDims > 0) then
!                     mem % simple_int_arr(:) = ptr % data % simple_int_arr(:)
!                  else
!                     mem % simple_int = ptr % data % simple_int
!                  end if
!
!               else if (ptr % contentsType == WRFJEDI_POOL_CONFIG) then
!
!                  dptr => ptr % data
!
!                  mem => pool_get_member(destPool, ptr % key, WRFJEDI_POOL_CONFIG)
!                  if (dptr % contentsType == WRFJEDI_POOL_REAL) then
!                     mem % simple_real = dptr % simple_real
!                  else if (dptr % contentsType == WRFJEDI_POOL_INTEGER) then
!                     mem % simple_int = dptr % simple_int
!                  else if (dptr % contentsType == WRFJEDI_POOL_CHARACTER) then
!                     mem % simple_char = dptr % simple_char
!                  else if (dptr % contentsType == WRFJEDI_POOL_LOGICAL) then
!                     mem % simple_logical = dptr % simple_logical
!                  end if
!
!               else if (ptr % contentsType == WRFJEDI_POOL_FIELD) then
!
!                  dptr => ptr % data
!
!                  ! Do this through brute force...
!                  mem => pool_get_member(destPool, ptr % key, WRFJEDI_POOL_FIELD)
!                  if (associated(dptr % r0)) then
!                     call wrfjedi_duplicate_field(dptr % r0, mem % r0, copy_array_only=.true.)
!                  else if (associated(dptr % r1)) then
!                     call wrfjedi_duplicate_field(dptr % r1, mem % r1, copy_array_only=.true.)
!                  else if (associated(dptr % r2)) then
!                     call wrfjedi_duplicate_field(dptr % r2, mem % r2, copy_array_only=.true.)
!                  else if (associated(dptr % r3)) then
!                     call wrfjedi_duplicate_field(dptr % r3, mem % r3, copy_array_only=.true.)
!                  else if (associated(dptr % r4)) then
!                     call wrfjedi_duplicate_field(dptr % r4, mem % r4, copy_array_only=.true.)
!                  else if (associated(dptr % r5)) then
!                     call wrfjedi_duplicate_field(dptr % r5, mem % r5, copy_array_only=.true.)
!                  else if (associated(dptr % i0)) then
!                     call wrfjedi_duplicate_field(dptr % i0, mem % i0, copy_array_only=.true.)
!                  else if (associated(dptr % i1)) then
!                     call wrfjedi_duplicate_field(dptr % i1, mem % i1, copy_array_only=.true.)
!                  else if (associated(dptr % i2)) then
!                     call wrfjedi_duplicate_field(dptr % i2, mem % i2, copy_array_only=.true.)
!                  else if (associated(dptr % i3)) then
!                     call wrfjedi_duplicate_field(dptr % i3, mem % i3, copy_array_only=.true.)
!                  else if (associated(dptr % c0)) then
!                     call wrfjedi_duplicate_field(dptr % c0, mem % c0, copy_array_only=.true.)
!                  else if (associated(dptr % c1)) then
!                     call wrfjedi_duplicate_field(dptr % c1, mem % c1, copy_array_only=.true.)
!                  else if (associated(dptr % l0)) then
!                     call wrfjedi_duplicate_field(dptr % l0, mem % l0, copy_array_only=.true.)
!                  else if (associated(dptr % r0a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % r0 => mem % r0a(j)
!                        call wrfjedi_duplicate_field(dptr % r0a(j), mem % r0, copy_array_only=.true.)
!                        nullify(mem % r0)
!                     end do
!                  else if (associated(dptr % r1a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % r1 => mem % r1a(j)
!                        call wrfjedi_duplicate_field(dptr % r1a(j), mem % r1, copy_array_only=.true.)
!                        nullify(mem % r1)
!                     end do
!                  else if (associated(dptr % r2a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % r2 => mem % r2a(j)
!                        call wrfjedi_duplicate_field(dptr % r2a(j), mem % r2, copy_array_only=.true.)
!                        nullify(mem % r2)
!                     end do
!                  else if (associated(dptr % r3a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % r3 => mem % r3a(j)
!                        call wrfjedi_duplicate_field(dptr % r3a(j), mem % r3, copy_array_only=.true.)
!                        nullify(mem % r3)
!                     end do
!                  else if (associated(dptr % r4a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % r4 => mem % r4a(j)
!                        call wrfjedi_duplicate_field(dptr % r4a(j), mem % r4, copy_array_only=.true.)
!                        nullify(mem % r4)
!                     end do
!                  else if (associated(dptr % r5a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % r5 => mem % r5a(j)
!                        call wrfjedi_duplicate_field(dptr % r5a(j), mem % r5, copy_array_only=.true.)
!                        nullify(mem % r5)
!                     end do
!                  else if (associated(dptr % i0a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % i0 => mem % i0a(j)
!                        call wrfjedi_duplicate_field(dptr % i0a(j), mem % i0, copy_array_only=.true.)
!                        nullify(mem % i0)
!                     end do
!                  else if (associated(dptr % i1a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % i1 => mem % i1a(j)
!                        call wrfjedi_duplicate_field(dptr % i1a(j), mem % i1, copy_array_only=.true.)
!                        nullify(mem % i1)
!                     end do
!                  else if (associated(dptr % i2a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % i2 => mem % i2a(j)
!                        call wrfjedi_duplicate_field(dptr % i2a(j), mem % i2, copy_array_only=.true.)
!                        nullify(mem % i2)
!                     end do
!                  else if (associated(dptr % i3a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % i3 => mem % i3a(j)
!                        call wrfjedi_duplicate_field(dptr % i3a(j), mem % i3, copy_array_only=.true.)
!                        nullify(mem % i3)
!                     end do
!                  else if (associated(dptr % c0a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % c0 => mem % c0a(j)
!                        call wrfjedi_duplicate_field(dptr % c0a(j), mem % c0, copy_array_only=.true.)
!                        nullify(mem % c0)
!                     end do
!                  else if (associated(dptr % c1a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % c1 => mem % c1a(j)
!                        call wrfjedi_duplicate_field(dptr % c1a(j), mem % c1, copy_array_only=.true.)
!                        nullify(mem % c1)
!                     end do
!                  else if (associated(dptr % l0a)) then
!                     do j=1,mem % contentsTimeLevs
!                        mem % l0 => mem % l0a(j)
!                        call wrfjedi_duplicate_field(dptr % l0a(j), mem % l0, copy_array_only=.true.)
!                        nullify(mem % l0)
!                     end do
!                  else
!                     call pool_mesg('While copying pool, member '//trim(ptr % key)//' has no valid field pointers.')
!                  end if
!
!               else if (ptr % contentsType == WRFJEDI_POOL_SUBPOOL) then
!
!                   mem => pool_get_member(destPool, ptr % key, WRFJEDI_POOL_SUBPOOL)
!                   call wrfjedi_pool_copy_pool(ptr % data % p, mem % p)
!
!               end if
!
!               ptr => ptr % next
!            end do
!
!         end do
!      end if
!
!   end subroutine wrfjedi_pool_copy_pool!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_initialize_time_levels
!!
!!> \brief WRFJEDI Pool copy routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine copies the data from the first time level of every field into
!!> all subsequent time levels, to initialize them with real values.
!!
!!-----------------------------------------------------------------------
!   recursive subroutine wrfjedi_pool_initialize_time_levels(inPool)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), pointer :: inPool
!
!      integer :: i, j
!      type (wrfjedi_pool_member_type), pointer :: ptr
!      type (fieldlist), pointer :: dptr
!      type (fieldlist), pointer :: mem
!      type (wrfjedi_pool_type), pointer :: subPool
!      type (wrfjedi_pool_iterator_type) :: itr
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      call wrfjedi_pool_begin_iteration(inPool)
!      do while (wrfjedi_pool_get_next_member(inPool, itr))
!         if (itr % memberType == WRFJEDI_POOL_SUBPOOL) then
!            call wrfjedi_pool_get_subpool(inPool, itr % memberName, subPool)
!            call wrfjedi_pool_initialize_time_levels(subPool)
!         else if (itr % memberType == WRFJEDI_POOL_FIELD) then
!            if ( threadNum == 0 ) then
!               if (itr % nTimeLevels > 1) then
!                  mem => pool_get_member(inPool, itr % memberName, WRFJEDI_POOL_FIELD)
!                  if (itr % dataType == WRFJEDI_POOL_REAL) then
!                     if (itr % nDims == 0) then
!                        do i = 2, itr % nTimeLevels
!                           mem % r0 => mem % r0a(i)
!                           call wrfjedi_duplicate_field(mem % r0a(1), mem % r0, copy_array_only=.true.)
!                           nullify(mem % r0)
!                        end do
!                     else if (itr % nDims == 1) then
!                        do i = 2, itr % nTimeLevels
!                           mem % r1 => mem % r1a(i)
!                           call wrfjedi_duplicate_field(mem % r1a(1), mem % r1, copy_array_only=.true.)
!                           nullify(mem % r1)
!                        end do
!                     else if (itr % nDims == 2) then
!                        do i = 2, itr % nTimeLevels
!                           mem % r2 => mem % r2a(i)
!                           call wrfjedi_duplicate_field(mem % r2a(1), mem % r2, copy_array_only=.true.)
!                           nullify(mem % r2)
!                        end do
!                     else if (itr % nDims == 3) then
!                        do i = 2, itr % nTimeLevels
!                           mem % r3 => mem % r3a(i)
!                           call wrfjedi_duplicate_field(mem % r3a(1), mem % r3, copy_array_only=.true.)
!                           nullify(mem % r3)
!                        end do
!                     else if (itr % nDims == 4) then
!                        do i = 2, itr % nTimeLevels
!                           mem % r4 => mem % r4a(i)
!                           call wrfjedi_duplicate_field(mem % r4a(1), mem % r4, copy_array_only=.true.)
!                           nullify(mem % r4)
!                        end do
!                     else if (itr % nDims == 5) then
!                        do i = 2, itr % nTimeLevels
!                           mem % r5 => mem % r5a(i)
!                           call wrfjedi_duplicate_field(mem % r5a(1), mem % r5, copy_array_only=.true.)
!                           nullify(mem % r5)
!                        end do
!                     end if
!                  else if (itr % dataType == WRFJEDI_POOL_INTEGER) then
!                     if (itr % nDims == 0) then
!                        do i = 2, itr % nTimeLevels
!                           mem % i0 => mem % i0a(i)
!                           call wrfjedi_duplicate_field(mem % i0a(1), mem % i0, copy_array_only=.true.)
!                           nullify(mem % i0)
!                        end do
!                     else if (itr % nDims == 1) then
!                        do i = 2, itr % nTimeLevels
!                           mem % i1 => mem % i1a(i)
!                           call wrfjedi_duplicate_field(mem % i1a(1), mem % i1, copy_array_only=.true.)
!                           nullify(mem % i1)
!                        end do
!                     else if (itr % nDims == 2) then
!                        do i = 2, itr % nTimeLevels
!                           mem % i2 => mem % i2a(i)
!                           call wrfjedi_duplicate_field(mem % i2a(1), mem % i2, copy_array_only=.true.)
!                           nullify(mem % i2)
!                        end do
!                     else if (itr % nDims == 3) then
!                        do i = 2, itr % nTimeLevels
!                           mem % i3 => mem % i3a(i)
!                           call wrfjedi_duplicate_field(mem % i3a(1), mem % i3, copy_array_only=.true.)
!                           nullify(mem % i3)
!                        end do
!                     end if
!                  else if (itr % dataType == WRFJEDI_POOL_CHARACTER) then
!                     if (itr % nDims == 0) then
!                        do i = 2, itr % nTimeLevels
!                           mem % c0 => mem % c0a(i)
!                           call wrfjedi_duplicate_field(mem % c0a(1), mem % c0, copy_array_only=.true.)
!                           nullify(mem % c0)
!                        end do
!                     else if (itr % nDims == 1) then
!                        do i = 2, itr % nTimeLevels
!                           mem % c1 => mem % c1a(i)
!                           call wrfjedi_duplicate_field(mem % c1a(1), mem % c1, copy_array_only=.true.)
!                           nullify(mem % c1)
!                        end do
!                     end if
!                  end if
!               end if
!            end if
!         end if
!      end do
!
!   end subroutine wrfjedi_pool_initialize_time_levels!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_link_pools
!!
!!> \brief WRFJEDI Pool link pools routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine links the fields in three pools together.
!!> It assumes all three pools contain the same field members.
!!> It will also link subpool fields.
!!
!!-----------------------------------------------------------------------
!   recursive subroutine wrfjedi_pool_link_pools(inPool, prevPool, nextPool)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), pointer :: inPool
!      type (wrfjedi_pool_type), pointer, optional :: prevPool, nextPool
!
!      integer :: i, j
!      type (wrfjedi_pool_type), pointer :: subPool, prevSubPool, nextSubPool
!      type (fieldlist), pointer :: poolMem, prevPoolMem, nextPoolMem
!      type (wrfjedi_pool_iterator_type) :: poolItr
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      nullify(prevSubPool)
!      nullify(nextSubPool)
!      nullify(prevPoolMem)
!      nullify(nextPoolMem)
!
!      call wrfjedi_pool_begin_iteration(inPool)
!      do while (wrfjedi_pool_get_next_member(inPool, poolItr))
!         ! Link subpools
!         if (poolItr % memberType == WRFJEDI_POOL_SUBPOOL) then
!            call wrfjedi_pool_get_subpool(inPool, poolItr % memberName, subPool)
!            if (present(prevPool)) then
!               call wrfjedi_pool_get_subpool(prevPool, poolItr % memberName, prevSubPool)
!            end if
!
!            if (present(nextPool)) then
!               call wrfjedi_pool_get_subpool(nextPool, poolItr % memberName, nextSubPool)
!            end if
!
!            if (associated(prevSubPool) .and. associated(nextSubPool)) then
!               call wrfjedi_pool_link_pools(subPool, prevSubPool, nextSubPool)
!            else if (associated(prevSubPool)) then
!               call wrfjedi_pool_link_pools(subPool, prevSubPool)
!            else if (associated(nextSubPool)) then
!               call wrfjedi_pool_link_pools(subPool, nextPool=nextSubPool)
!            else
!               call wrfjedi_pool_link_pools(subPool)
!            end if
!
!         ! Link fields
!         else if (poolItr % memberType == WRFJEDI_POOL_FIELD) then
!            
!            if ( threadNum == 0 ) then
!               poolMem => pool_get_member(inPool, poolItr % memberName, WRFJEDI_POOL_FIELD)
!               if (present(prevPool)) then
!                   prevPoolMem => pool_get_member(prevPool, poolItr % memberName, WRFJEDI_POOL_FIELD)
!               end if
!
!               if (present(nextPool)) then
!                   nextPoolMem => pool_get_member(nextPool, poolItr % memberName, WRFJEDI_POOL_FIELD)
!               end if
!
!               if (poolItr % dataType == WRFJEDI_POOL_REAL) then
!                  if (poolItr % nDims == 0) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % r0a(i) % prev => prevPoolMem % r0a(i)
!                           else
!                              nullify(poolMem % r0a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % r0a(i) % next => nextPoolMem % r0a(i)
!                           else
!                              nullify(poolMem % r0a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % r0 % prev => prevPoolMem % r0
!                        else
!                           nullify(poolMem % r0 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % r0 % next => nextPoolMem % r0
!                        else
!                           nullify(poolMem % r0 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 1) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % r1a(i) % prev => prevPoolMem % r1a(i)
!                           else
!                              nullify(poolMem % r1a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % r1a(i) % next => nextPoolMem % r1a(i)
!                           else
!                              nullify(poolMem % r1a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % r1 % prev => prevPoolMem % r1
!                        else
!                           nullify(poolMem % r1 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % r1 % next => nextPoolMem % r1
!                        else
!                           nullify(poolMem % r1 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 2) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % r2a(i) % prev => prevPoolMem % r2a(i)
!                           else
!                              nullify(poolMem % r2a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % r2a(i) % next => nextPoolMem % r2a(i)
!                           else
!                              nullify(poolMem % r2a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % r2 % prev => prevPoolMem % r2
!                        else
!                           nullify(poolMem % r2 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % r2 % next => nextPoolMem % r2
!                        else
!                           nullify(poolMem % r2 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 3) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % r3a(i) % prev => prevPoolMem % r3a(i)
!                           else
!                              nullify(poolMem % r3a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % r3a(i) % next => nextPoolMem % r3a(i)
!                           else
!                              nullify(poolMem % r3a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % r3 % prev => prevPoolMem % r3
!                        else
!                           nullify(poolMem % r3 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % r3 % next => nextPoolMem % r3
!                        else
!                           nullify(poolMem % r3 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 4) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % r4a(i) % prev => prevPoolMem % r4a(i)
!                           else
!                              nullify(poolMem % r4a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % r4a(i) % next => nextPoolMem % r4a(i)
!                           else
!                              nullify(poolMem % r4a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % r4 % prev => prevPoolMem % r4
!                        else
!                           nullify(poolMem % r4 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % r4 % next => nextPoolMem % r4
!                        else
!                           nullify(poolMem % r4 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 5) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % r5a(i) % prev => prevPoolMem % r5a(i)
!                           else
!                              nullify(poolMem % r5a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % r5a(i) % next => nextPoolMem % r5a(i)
!                           else
!                              nullify(poolMem % r5a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % r5 % prev => prevPoolMem % r5
!                        else
!                           nullify(poolMem % r5 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % r5 % next => nextPoolMem % r5
!                        else
!                           nullify(poolMem % r5 % next)
!                        end if
!                     end if
!                  end if
!               else if (poolItr % dataType == WRFJEDI_POOL_INTEGER) then
!                  if (poolItr % nDims == 0) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % i0a(i) % prev => prevPoolMem % i0a(i)
!                           else
!                              nullify(poolMem % i0a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % i0a(i) % next => nextPoolMem % i0a(i)
!                           else
!                              nullify(poolMem % i0a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % i0 % prev => prevPoolMem % i0
!                        else
!                           nullify(poolMem % i0 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % i0 % next => nextPoolMem % i0
!                        else
!                           nullify(poolMem % i0 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 1) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % i1a(i) % prev => prevPoolMem % i1a(i)
!                           else
!                              nullify(poolMem % i1a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % i1a(i) % next => nextPoolMem % i1a(i)
!                           else
!                              nullify(poolMem % i1a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % i1 % prev => prevPoolMem % i1
!                        else
!                           nullify(poolMem % i1 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % i1 % next => nextPoolMem % i1
!                        else
!                           nullify(poolMem % i1 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 2) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % i2a(i) % prev => prevPoolMem % i2a(i)
!                           else
!                              nullify(poolMem % i2a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % i2a(i) % next => nextPoolMem % i2a(i)
!                           else
!                              nullify(poolMem % i2a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % i2 % prev => prevPoolMem % i2
!                        else
!                           nullify(poolMem % i2 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % i2 % next => nextPoolMem % i2
!                        else
!                           nullify(poolMem % i2 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 3) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % i3a(i) % prev => prevPoolMem % i3a(i)
!                           else
!                              nullify(poolMem % i3a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % i3a(i) % next => nextPoolMem % i3a(i)
!                           else
!                              nullify(poolMem % i3a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % i3 % prev => prevPoolMem % i3
!                        else
!                           nullify(poolMem % i3 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % i3 % next => nextPoolMem % i3
!                        else
!                           nullify(poolMem % i3 % next)
!                        end if
!                     end if
!                  end if
!               else if (poolItr % dataType == WRFJEDI_POOL_CHARACTER) then
!                  if (poolItr % nDims == 0) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % c0a(i) % prev => prevPoolMem % c0a(i)
!                           else
!                              nullify(poolMem % c0a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % c0a(i) % next => nextPoolMem % c0a(i)
!                           else
!                              nullify(poolMem % c0a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % c0 % prev => prevPoolMem % c0
!                        else
!                           nullify(poolMem % c0 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % c0 % next => nextPoolMem % c0
!                        else
!                           nullify(poolMem % c0 % next)
!                        end if
!                     end if
!                  else if (poolItr % nDims == 1) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           if (associated(prevPoolMem)) then
!                              poolMem % c1a(i) % prev => prevPoolMem % c1a(i)
!                           else
!                              nullify(poolMem % c1a(i) % prev)
!                           end if
!
!                           if (associated(nextPoolMem)) then
!                              poolMem % c1a(i) % next => nextPoolMem % c1a(i)
!                           else
!                              nullify(poolMem % c1a(i) % next)
!                           end if
!                        end do
!                     else
!                        if (associated(prevPoolMem)) then
!                           poolMem % c1 % prev => prevPoolMem % c1
!                        else
!                           nullify(poolMem % c1 % prev)
!                        end if
!
!                        if (associated(nextPoolMem)) then
!                           poolMem % c1 % next => nextPoolMem % c1
!                        else
!                           nullify(poolMem % c1 % next)
!                        end if
!                     end if
!                  end if
!               end if
!            end if
!         end if
!      end do
!
!   end subroutine wrfjedi_pool_link_pools!}}}
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_link_parinfo
!!
!!> \brief WRFJEDI Pool link parinfo in fields routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine links the parallel info exchange lists for pool members.
!!
!!-----------------------------------------------------------------------
!   recursive subroutine wrfjedi_pool_link_parinfo(block, inPool)!{{{
!
!      implicit none
!
!      type (block_type), intent(in) :: block
!      type (wrfjedi_pool_type), pointer :: inPool
!
!      integer :: i, j, decompType
!      type (wrfjedi_pool_type), pointer :: subPool
!      type (fieldlist), pointer :: poolMem
!      type (wrfjedi_pool_iterator_type) :: poolItr
!      character (len=StrKIND), dimension(:), pointer :: dimNames
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      call wrfjedi_pool_begin_iteration(inPool)
!      do while (wrfjedi_pool_get_next_member(inPool, poolItr))
!         ! Link subpools
!         if (poolItr % memberType == WRFJEDI_POOL_SUBPOOL) then
!            call wrfjedi_pool_get_subpool(inPool, poolItr % memberName, subPool)
!            call wrfjedi_pool_link_parinfo(block, subPool)
!
!         ! Link fields
!         else if (poolItr % memberType == WRFJEDI_POOL_FIELD) then
!            if ( threadNum == 0 ) then
!               decompType = -1
!
!               poolMem => pool_get_member(inPool, poolItr % memberName, WRFJEDI_POOL_FIELD)
!               
!               if (poolItr % dataType == WRFJEDI_POOL_REAL) then
!                  if (poolItr % nDims == 0) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           nullify(poolMem % r0a(i) % sendList)
!                           nullify(poolMem % r0a(i) % recvList)
!                           nullify(poolMem % r0a(i) % copyList)
!                        end do
!                     else
!                        nullify(poolMem % r0 % sendList)
!                        nullify(poolMem % r0 % recvList)
!                        nullify(poolMem % r0 % copyList)
!                     end if
!                  else if (poolItr % nDims == 1) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % r1a(1) % dimNames(1))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r1a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % r1a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % r1a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r1a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % r1a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % r1a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r1a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % r1a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % r1a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % r1 % dimNames(1))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % r1 % sendList => block % parinfo % cellsToSend
!                           poolMem % r1 % recvList => block % parinfo % cellsToRecv
!                           poolMem % r1 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % r1 % sendList => block % parinfo % edgesToSend
!                           poolMem % r1 % recvList => block % parinfo % edgesToRecv
!                           poolMem % r1 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % r1 % sendList => block % parinfo % verticesToSend
!                           poolMem % r1 % recvList => block % parinfo % verticesToRecv
!                           poolMem % r1 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  else if (poolItr % nDims == 2) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % r2a(1) % dimNames(2))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r2a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % r2a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % r2a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r2a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % r2a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % r2a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r2a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % r2a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % r2a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % r2 % dimNames(2))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % r2 % sendList => block % parinfo % cellsToSend
!                           poolMem % r2 % recvList => block % parinfo % cellsToRecv
!                           poolMem % r2 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % r2 % sendList => block % parinfo % edgesToSend
!                           poolMem % r2 % recvList => block % parinfo % edgesToRecv
!                           poolMem % r2 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % r2 % sendList => block % parinfo % verticesToSend
!                           poolMem % r2 % recvList => block % parinfo % verticesToRecv
!                           poolMem % r2 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  else if (poolItr % nDims == 3) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % r3a(1) % dimNames(3))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r3a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % r3a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % r3a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r3a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % r3a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % r3a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r3a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % r3a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % r3a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % r3 % dimNames(3))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % r3 % sendList => block % parinfo % cellsToSend
!                           poolMem % r3 % recvList => block % parinfo % cellsToRecv
!                           poolMem % r3 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % r3 % sendList => block % parinfo % edgesToSend
!                           poolMem % r3 % recvList => block % parinfo % edgesToRecv
!                           poolMem % r3 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % r3 % sendList => block % parinfo % verticesToSend
!                           poolMem % r3 % recvList => block % parinfo % verticesToRecv
!                           poolMem % r3 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  else if (poolItr % nDims == 4) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % r4 % dimNames(4))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r4a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % r4a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % r4a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r4a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % r4a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % r4a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r4a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % r4a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % r4a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % r4 % dimNames(4))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % r4 % sendList => block % parinfo % cellsToSend
!                           poolMem % r4 % recvList => block % parinfo % cellsToRecv
!                           poolMem % r4 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % r4 % sendList => block % parinfo % edgesToSend
!                           poolMem % r4 % recvList => block % parinfo % edgesToRecv
!                           poolMem % r4 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % r4 % sendList => block % parinfo % verticesToSend
!                           poolMem % r4 % recvList => block % parinfo % verticesToRecv
!                           poolMem % r4 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  else if (poolItr % nDims == 5) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % r5a(1) % dimNames(5))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r5a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % r5a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % r5a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r5a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % r5a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % r5a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % r5a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % r5a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % r5a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % r5 % dimNames(5))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % r5 % sendList => block % parinfo % cellsToSend
!                           poolMem % r5 % recvList => block % parinfo % cellsToRecv
!                           poolMem % r5 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % r5 % sendList => block % parinfo % edgesToSend
!                           poolMem % r5 % recvList => block % parinfo % edgesToRecv
!                           poolMem % r5 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % r5 % sendList => block % parinfo % verticesToSend
!                           poolMem % r5 % recvList => block % parinfo % verticesToRecv
!                           poolMem % r5 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  end if
!               else if (poolItr % dataType == WRFJEDI_POOL_INTEGER) then
!                  if (poolItr % nDims == 0) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           nullify(poolMem % i0a(i) % sendList)
!                           nullify(poolMem % i0a(i) % recvList)
!                           nullify(poolMem % i0a(i) % copyList)
!                        end do
!                     else
!                        nullify(poolMem % i0 % sendList)
!                        nullify(poolMem % i0 % recvList)
!                        nullify(poolMem % i0 % copyList)
!                     end if
!                  else if (poolItr % nDims == 1) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % i1a(1) % dimNames(1))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i1a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % i1a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % i1a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i1a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % i1a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % i1a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i1a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % i1a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % i1a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % i1 % dimNames(1))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % i1 % sendList => block % parinfo % cellsToSend
!                           poolMem % i1 % recvList => block % parinfo % cellsToRecv
!                           poolMem % i1 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % i1 % sendList => block % parinfo % edgesToSend
!                           poolMem % i1 % recvList => block % parinfo % edgesToRecv
!                           poolMem % i1 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % i1 % sendList => block % parinfo % verticesToSend
!                           poolMem % i1 % recvList => block % parinfo % verticesToRecv
!                           poolMem % i1 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  else if (poolItr % nDims == 2) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % i2a(1) % dimNames(2))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i2a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % i2a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % i2a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i2a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % i2a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % i2a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i2a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % i2a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % i2a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % i2 % dimNames(2))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % i2 % sendList => block % parinfo % cellsToSend
!                           poolMem % i2 % recvList => block % parinfo % cellsToRecv
!                           poolMem % i2 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % i2 % sendList => block % parinfo % edgesToSend
!                           poolMem % i2 % recvList => block % parinfo % edgesToRecv
!                           poolMem % i2 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % i2 % sendList => block % parinfo % verticesToSend
!                           poolMem % i2 % recvList => block % parinfo % verticesToRecv
!                           poolMem % i2 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  else if (poolItr % nDims == 3) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % i3a(1) % dimNames(3))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i3a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % i3a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % i3a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i3a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % i3a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % i3a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % i3a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % i3a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % i3a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % i3 % dimNames(3))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % i3 % sendList => block % parinfo % cellsToSend
!                           poolMem % i3 % recvList => block % parinfo % cellsToRecv
!                           poolMem % i3 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % i3 % sendList => block % parinfo % edgesToSend
!                           poolMem % i3 % recvList => block % parinfo % edgesToRecv
!                           poolMem % i3 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % i3 % sendList => block % parinfo % verticesToSend
!                           poolMem % i3 % recvList => block % parinfo % verticesToRecv
!                           poolMem % i3 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  end if
!               else if (poolItr % dataType == WRFJEDI_POOL_CHARACTER) then
!                  if (poolItr % nDims == 0) then
!                     if (poolItr % nTimeLevels > 1) then
!                        do i = 1, poolItr % nTimeLevels
!                           nullify(poolMem % c0a(i) % sendList)
!                           nullify(poolMem % c0a(i) % recvList)
!                           nullify(poolMem % c0a(i) % copyList)
!                        end do
!                     else
!                        nullify(poolMem % c0 % sendList)
!                        nullify(poolMem % c0 % recvList)
!                        nullify(poolMem % c0 % copyList)
!                     end if
!                  else if (poolItr % nDims == 1) then
!                     if (poolItr % nTimeLevels > 1) then
!                        decompType = pool_get_member_decomp_type(poolMem % c1a(1) % dimNames(1))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % c1a(i) % sendList => block % parinfo % cellsToSend
!                              poolMem % c1a(i) % recvList => block % parinfo % cellsToRecv
!                              poolMem % c1a(i) % copyList => block % parinfo % cellsToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % c1a(i) % sendList => block % parinfo % edgesToSend
!                              poolMem % c1a(i) % recvList => block % parinfo % edgesToRecv
!                              poolMem % c1a(i) % copyList => block % parinfo % edgesToCopy
!                           end do
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           do i = 1, poolItr % nTimeLevels
!                              poolMem % c1a(i) % sendList => block % parinfo % verticesToSend
!                              poolMem % c1a(i) % recvList => block % parinfo % verticesToRecv
!                              poolMem % c1a(i) % copyList => block % parinfo % verticesToCopy
!                           end do
!                        end if
!                     else
!                        decompType = pool_get_member_decomp_type(poolMem % c1 % dimNames(1))
!
!                        if (decompType == WRFJEDI_DECOMP_CELLS) then
!                           poolMem % c1 % sendList => block % parinfo % cellsToSend
!                           poolMem % c1 % recvList => block % parinfo % cellsToRecv
!                           poolMem % c1 % copyList => block % parinfo % cellsToCopy
!                        else if (decompType == WRFJEDI_DECOMP_EDGES) then
!                           poolMem % c1 % sendList => block % parinfo % edgesToSend
!                           poolMem % c1 % recvList => block % parinfo % edgesToRecv
!                           poolMem % c1 % copyList => block % parinfo % edgesToCopy
!                        else if (decompType == WRFJEDI_DECOMP_VERTICES) then
!                           poolMem % c1 % sendList => block % parinfo % verticesToSend
!                           poolMem % c1 % recvList => block % parinfo % verticesToRecv
!                           poolMem % c1 % copyList => block % parinfo % verticesToCopy
!                        end if
!                     end if
!                  end if
!               end if
!            end if
!         end if
!      end do
!
!   end subroutine wrfjedi_pool_link_parinfo!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_0d_int
!
!> \brief WRFJEDI Pool get 0D Integer field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 0D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_0d_int(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field0DInteger), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (infield % Ndim /= 0) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 0-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % ifield_1d),maxval(infield % ifield_1d)
         field%array => infield % ifield_0d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_0d_int!}}}
!

!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_1d_int
!
!> \brief WRFJEDI Pool get 1D Integer field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 1D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_1d_int(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field1DInteger), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (infield % Ndim /= 1) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 1-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % ifield_1d),maxval(infield % ifield_1d)
         field%array => infield % ifield_1d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_1d_int!}}}
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_2d_int
!
!> \brief WRFJEDI Pool get 2D Integer field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 2D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_2d_int(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field2DInteger), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (infield % Ndim /= 2) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 2-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % ifield_2d),maxval(infield % ifield_2d)
         field%array => infield % ifield_2d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_2d_int!}}}
!

!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_3d_int
!
!> \brief WRFJEDI Pool get 3D Integer field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 3D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_3d_int(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field3DInteger), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (infield % Ndim /= 3) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 3-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % ifield_3d),maxval(infield % ifield_3d)
         field%array => infield % ifield_3d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_3d_int!}}}
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_4d_int
!
!> \brief WRFJEDI Pool get 4D Integer field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 4D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_4d_int(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field4DInteger), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (infield % Ndim /= 4) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 4-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % ifield_4d),maxval(infield % ifield_4d)
         field%array => infield % ifield_4d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_4d_int!}}}

!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_0d_real
!
!> \brief WRFJEDI Pool get 0D Real field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 0D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_0d_real(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field0DReal), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)
!         call field%printFieldHead()

         if (infield % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (infield % Ndim /= 0) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 0-d field.')
         end if
         field%array => infield % rfield_0d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_0d_real!}}}
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_1d_real
!
!> \brief WRFJEDI Pool get 1D Real field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 1D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_1d_real(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field1DReal), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (infield % Ndim /= 1) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 1-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % rfield_1d),maxval(infield % rfield_1d)
         field%array => infield % rfield_1d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_1d_real!}}}
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_2d_real
!
!> \brief WRFJEDI Pool get 2D Real field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 2D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_2d_real(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field2DReal), pointer :: field

!      write(*,*) 'in wrfjedi_pool_get_gridfield_2d_real:',trim(key)
!      write(*,*) 'in wrfjedi_pool_get_gridfield_2d_real:',infield%VarName
!      write(*,*) 'in wrfjedi_pool_get_gridfield_2d_real:',field%VarName

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)
!         call field%printFieldHead()

         if (infield % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (infield % Ndim /= 2) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 2-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % rfield_2d),maxval(infield % rfield_2d)
         field%array => infield % rfield_2d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_2d_real!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_3d_real
!
!> \brief WRFJEDI Pool get 3D Real field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 3D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_3d_real(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field3DReal), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)
!         call field%printFieldHead()

         if (infield % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (infield % Ndim /= 3) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 3-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % rfield_3d),maxval(infield % rfield_3d)
         field%array => infield % rfield_3d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_3d_real!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_get_gridfield_4d_real
!
!> \brief WRFJEDI Pool get 4D Real field from grid field
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine read field from grid type when field is a 4D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_gridfield_4d_real(infield, key, field)!{{{
!

      implicit none

      type (fieldlist), intent(in), pointer :: infield
      character (len=*), intent(in) :: key
      type (field4DReal), pointer :: field

      nullify(field%array)
      if (associated(infield)) then

         call field%fillFieldHead(infield)

         if (infield % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (infield % Ndim /= 4) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 4-d field.')
         end if
!         write(*,*) 'MIN/MAX value: ', minval(infield % rfield_4d),maxval(infield % rfield_4d)
         field%array => infield % rfield_4d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in grid field.')

      end if

   end subroutine wrfjedi_pool_get_gridfield_4d_real!}}}
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_0d_real
!
!> \brief WRFJEDI Pool 0D Real field add routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This routine inserts field into inPool when field is a 0D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_0d_real(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field0DReal), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
!         call field%printFieldHead()
         newmem % data % Type = WRFJEDI_POOL_REAL
         newmem % data % Ndim = 0

         newmem % data % rfield_0d => field % array
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_0d_real!}}}

!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_1d_real
!
!> \brief WRFJEDI Pool 1D Real field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 1D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_1d_real(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field1DReal), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_REAL
         newmem % data % Ndim = 1

         newmem % data % rfield_1d => field % array
    
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_1d_real!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_2d_real
!
!> \brief WRFJEDI Pool 2D Real field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 2D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_2d_real(inPool, key, field)!{{{
!
      implicit none
!
      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field2DReal), pointer :: field
!
      real(kind=RKIND), allocatable, target :: array(:,:)
      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum
      integer :: ierr
!
      threadNum = wrfjedi_threading_get_thread_num()
!
      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD
!
         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_REAL
         newmem % data % Ndim = 2
         
         newmem % data % rfield_2d => field % array
!         write(*,*) 'maxmin=',maxval(newmem % data % rfield_2d), &
!                              minval(newmem % data % rfield_2d)
!   
         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data % rfield_2d)
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if
!
   end subroutine wrfjedi_pool_add_field_2d_real!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_3d_real
!
!> \brief WRFJEDI Pool 3D Real field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 3D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_3d_real(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field3DReal), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_REAL
         newmem % data % Ndim = 3
         newmem % data % rfield_3d => field % array
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_3d_real!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_4d_real
!
!> \brief WRFJEDI Pool 4D Real field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 4D real field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_4d_real(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field4DReal), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_REAL
         newmem % data % Ndim = 4
         newmem % data % rfield_4d => field % array
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_4d_real!}}}

!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_0d_int
!
!> \brief WRFJEDI Pool 1D Integer field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 0D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_0d_int(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field0DInteger), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_INTEGER
         newmem % data % Ndim = 0
         newmem % data % ifield_0d => field % array

         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_0d_int!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_1d_int
!
!> \brief WRFJEDI Pool 1D Integer field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 1D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_1d_int(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field1DInteger), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_INTEGER
         newmem % data % Ndim = 1
         newmem % data % ifield_1d => field % array
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_1d_int!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_2d_int
!
!> \brief WRFJEDI Pool 2D Integer field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 2D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_2d_int(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field2DInteger), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_INTEGER
         newmem % data % Ndim = 2
         newmem % data % ifield_2d => field % array
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_2d_int!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_3d_int
!
!> \brief WRFJEDI Pool 3D Integer field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 3D integer field
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_add_field_3d_int(inPool, key, field)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (field3DInteger), pointer :: field

      type (wrfjedi_pool_member_type), pointer :: newmem
      integer :: threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         allocate(newmem)
         newmem % key = trim(key)
         newmem % keyLen = len_trim(key)
         newmem % contentsType = WRFJEDI_POOL_FIELD

         allocate(newmem % data)
         call wrfjedi_nullify_fieldlist(newmem % data)
         call field%sendFieldHead(newmem % data)
         newmem % data % Type = WRFJEDI_POOL_INTEGER
         newmem % data % Ndim = 3
         newmem % data % ifield_3d => field % array
   
         if (.not. pool_add_member(inPool, key, newmem)) then
            deallocate(newmem % data)
            deallocate(newmem)
         end if
      end if

   end subroutine wrfjedi_pool_add_field_3d_int!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_add_field_0d_char
!
!> \brief WRFJEDI Pool 0D Character field add routine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This routine inserts field into inPool when field is a 0D character field
!
!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_0d_char(inPool, key, field)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field0DChar), pointer :: field
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_CHARACTER
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 1
!         newmem % data % c0 => field
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_field_0d_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_1d_char
!!
!!> \brief WRFJEDI Pool 1D Character field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts field into inPool when field is a 1D character field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_1d_char(inPool, key, field)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field1DChar), pointer :: field
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_CHARACTER
!         newmem % data % contentsDims = 1
!         newmem % data % contentsTimeLevs = 1
!         newmem % data % c1 => field
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_field_1d_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_0d_reals
!!
!!> \brief WRFJEDI Pool 0D Multi-level Real field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 0D real field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_0d_reals(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field0DReal), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % r0 => fields(1)
!         else
!            newmem % data % r0a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_0d_reals!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_1d_reals
!!
!!> \brief WRFJEDI Pool 1D Multi-level Real field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 1D real field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_1d_reals(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field1DReal), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 1
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % r1 => fields(1)
!         else
!            newmem % data % r1a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_1d_reals!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_2d_reals
!!
!!> \brief WRFJEDI Pool 2D Multi-level Real field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 2D real field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_2d_reals(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field2DReal), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 2
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % r2 => fields(1)
!         else
!            newmem % data % r2a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_2d_reals!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_3d_reals
!!
!!> \brief WRFJEDI Pool 3D Multi-level Real field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 3D real field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_3d_reals(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field3DReal), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 3
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % r3 => fields(1)
!         else
!            newmem % data % r3a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_3d_reals!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_4d_reals
!!
!!> \brief WRFJEDI Pool 4D Multi-level Real field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 4D real field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_4d_reals(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field4DReal), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 4
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % r4 => fields(1)
!         else
!            newmem % data % r4a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_4d_reals!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_5d_reals
!!
!!> \brief WRFJEDI Pool 5D Multi-level Real field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 5D real field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_5d_reals(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field5DReal), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 5
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % r5 => fields(1)
!         else
!            newmem % data % r5a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_5d_reals!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_0d_ints
!!
!!> \brief WRFJEDI Pool 0D Multi-level Integer field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 0D integer field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_0d_ints(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field0DInteger), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % i0 => fields(1)
!         else
!            newmem % data % i0a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_0d_ints!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_1d_ints
!!
!!> \brief WRFJEDI Pool 1D Multi-level Integer field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 1D integer field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_1d_ints(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field1DInteger), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % keyLen = len_trim(key)
!         newmem % key = trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!         nullify(newmem % next)
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 1
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % i1 => fields(1)
!         else
!            newmem % data % i1a => fields
!         end if
!      end if
!   
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_1d_ints!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_2d_ints
!!
!!> \brief WRFJEDI Pool 2D Multi-level integer field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 2D integer field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_2d_ints(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field2DInteger), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 2
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % i2 => fields(1)
!         else
!            newmem % data % i2a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_2d_ints!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_3d_ints
!!
!!> \brief WRFJEDI Pool 3D Multi-level Integer field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 3D integer field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_3d_ints(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field3DInteger), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 3
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % i3 => fields(1)
!         else
!            newmem % data % i3a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_3d_ints!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_0d_chars
!!
!!> \brief WRFJEDI Pool 0D Multi-level Character field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 0D character field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_0d_chars(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field0DChar), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_CHARACTER
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % c0 => fields(1)
!         else
!            newmem % data % c0a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_0d_chars!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_field_1d_chars
!!
!!> \brief WRFJEDI Pool 1D Multi-level Character field add routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts fields into inPool when fields is a multi-level 1D character field
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_field_1d_chars(inPool, key, fields)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (field1DChar), dimension(:), pointer :: fields
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_FIELD
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_CHARACTER
!         newmem % data % contentsDims = 1
!         newmem % data % contentsTimeLevs = size(fields)
!         if (newmem % data % contentsTimeLevs == 1) then
!            newmem % data % c1 => fields(1)
!         else
!            newmem % data % c1a => fields
!         end if
!      
!      end if
!      if (.not. pool_add_member(inPool, key, newmem)) then
!         deallocate(newmem % data)
!         deallocate(newmem)
!      end if
!
!   end subroutine wrfjedi_pool_add_field_1d_chars!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_field_info
!!
!!> \brief WRFJEDI Pool Field Information Query subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a data structure containing information related to the
!!>  field in inPool with the name key
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_field_info(inPool, key, info)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      type (wrfjedi_pool_field_info_type), intent(out) :: info
!
!      integer :: hash, endl
!      type (wrfjedi_pool_member_type), pointer :: ptr
!
!      info % fieldType = -1
!      info % nDims = -1
!      info % nTimeLevels = -1
!      info % nHaloLayers = -1
!      info % isActive = .false.
!
!      endl = len_trim(key)
!      call pool_hash(hash, key, endl)
!
!      hash = mod(hash, inPool % size) + 1
!
!      ptr => inPool % table(hash) % head
!      do while (associated(ptr))
!         if (ptr % contentsType == WRFJEDI_POOL_FIELD) then
!            if (endl == ptr % keyLen) then
!               if (key(1:endl) == ptr % key(1:endl)) then
!
!                  info % fieldType = ptr % data % contentsType
!                  info % nDims = ptr % data % contentsDims
!                  info % nTimeLevels = ptr % data % contentsTimeLevs
!                  info % nHaloLayers = 0
!
!                  if ( info % fieldType == WRFJEDI_POOL_REAL ) then
!                     if ( info % nDims == 0 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % r0a(1) % isActive
!                           if ( associated(ptr % data % r0a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r0a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % r0 % isActive
!                           if ( associated(ptr % data % r0 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r0 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 1 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % r1a(1) % isActive
!                           if ( associated(ptr % data % r1a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r1a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % r1 % isActive
!                           if ( associated(ptr % data % r1 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r1 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 2 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % r2a(1) % isActive
!                           if ( associated(ptr % data % r2a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r2a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % r2 % isActive
!                           if ( associated(ptr % data % r2 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r2 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 3 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % r3a(1) % isActive
!                           if ( associated(ptr % data % r3a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r3a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % r3 % isActive
!                           if ( associated(ptr % data % r3 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r3 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 4 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % r4a(1) % isActive
!                           if ( associated(ptr % data % r4a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r4a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % r4 % isActive
!                           if ( associated(ptr % data % r4 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r4 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 5 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % r5a(1) % isActive
!                           if ( associated(ptr % data % r5a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r5a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % r5 % isActive
!                           if ( associated(ptr % data % r5 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % r5 % sendList % halos)
!                           end if
!                        end if
!                     end if
!                  else if (info % fieldType == WRFJEDI_POOL_INTEGER ) then
!                     if ( info % nDims == 0 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % i0a(1) % isActive
!                           if ( associated(ptr % data % i0a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i0a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % i0 % isActive
!                           if ( associated(ptr % data % i0 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i0 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 1 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % i1a(1) % isActive
!                           if ( associated(ptr % data % i1a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i1a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % i1 % isActive
!                           if ( associated(ptr % data % i1 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i1 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 2 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % i2a(1) % isActive
!                           if ( associated(ptr % data % i2a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i2a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % i2 % isActive
!                           if ( associated(ptr % data % i2 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i2 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 3 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % i3a(1) % isActive
!                           if ( associated(ptr % data % i3a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i3a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % i3 % isActive
!                           if ( associated(ptr % data % i3 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % i3 % sendList % halos)
!                           end if
!                        end if
!                     end if
!                  else if (info % fieldType == WRFJEDI_POOL_CHARACTER ) then
!                     if ( info % nDims == 0 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % c0a(1) % isActive
!                           if ( associated(ptr % data % c0a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % c0a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % c0 % isActive
!                           if ( associated(ptr % data % c0 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % c0 % sendList % halos)
!                           end if
!                        end if
!                     else if ( info % nDims == 1 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % c1a(1) % isActive
!                           if ( associated(ptr % data % c1a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % c1a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % c1 % isActive
!                           if ( associated(ptr % data % c1 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % c1 % sendList % halos)
!                           end if
!                        end if
!                     end if
!                  else if (info % fieldType == WRFJEDI_POOL_LOGICAL ) then
!                     if ( info % nDims == 0 ) then
!                        if ( info % nTimeLevels > 1 ) then 
!                           info % isActive = ptr % data % l0a(1) % isActive
!                           if ( associated(ptr % data % l0a(1) % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % l0a(1) % sendList % halos)
!                           end if
!                        else
!                           info % isActive = ptr % data % l0 % isActive
!                           if ( associated(ptr % data % l0 % sendList) ) then
!                              info % nHaloLayers = size(ptr % data % l0 % sendList % halos)
!                           end if
!                        end if
!                     end if
!                  end if
!                  exit
!               end if
!            end if
!         end if
!         ptr => ptr % next
!      end do
!
!      if (.not. associated(ptr)) then
!         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_field_info!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_0d_real
!
!> \brief WRFJEDI Pool 0D Real field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_0d_real(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field0DReal), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (mem % Ndim /= 0) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 0-d field.')
         end if

         call field%fillFieldHead(mem)
         field%array => mem % rfield_0d
      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_0d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_1d_real
!
!> \brief WRFJEDI Pool 1D Real field get subroutine
!> \author Ming Hu
!> \date   11/19/2014
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_1d_real(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field1DReal), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (mem % Ndim /= 1) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 1-d field.')
         end if

         call field%fillFieldHead(mem)
         field%array => mem % rfield_1d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_1d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_2d_real
!
!> \brief WRFJEDI Pool 2D Real field get subroutine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_2d_real(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field2DReal), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (mem % Ndim /= 2) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 2-d field.')
         end if

          call field%fillFieldHead(mem)
          field%array => mem % rfield_2d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_2d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_3d_real
!
!> \brief WRFJEDI Pool 3D Real field get subroutine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_3d_real(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field3DReal), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (mem % Ndim /= 3) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 3-d field.')
         end if

         call field%fillFieldHead(mem)
!         call field%printFieldHead()
         field%array => mem % rfield_3d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_3d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_4d_real
!
!> \brief WRFJEDI Pool 4D Real field get subroutine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_4d_real(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field4DReal), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_REAL) then
            call pool_mesg('Error: Field '//trim(key)//' is not type real.')
         end if
         if (mem % Ndim /= 4) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 4-d field.')
         end if

          call field%fillFieldHead(mem)
          field%array => mem % rfield_4d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_4d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_0d_int
!
!> \brief WRFJEDI Pool 0D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_0d_int(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field0DInteger), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (mem % Ndim /= 0) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 0-d field.')
         end if

         call field%fillFieldHead(mem)
         field%array => mem % ifield_0d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_0d_int!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_1d_int
!
!> \brief WRFJEDI Pool 1D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_1d_int(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field1DInteger), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (mem % Ndim /= 1) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 1-d field.')
         end if

         call field%fillFieldHead(mem)
         field%array => mem % ifield_1d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_1d_int!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_2d_int
!
!> \brief WRFJEDI Pool 2D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_2d_int(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field2DInteger), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (mem % Ndim /= 2) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 2-d field.')
         end if

         call field%fillFieldHead(mem)
         field%array => mem % ifield_2d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_2d_int!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_field_3d_int
!
!> \brief WRFJEDI Pool 3D Integer field get subroutine
!> \author Ming Hu
!> \date   11/19/2018
!> \details
!> This subroutine returns a pointer to the field associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_field_3d_int(inPool, key, field, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      type (field3DInteger), pointer :: field
      integer, intent(in), optional :: timeLevel

      type (fieldlist), pointer :: mem
      integer :: local_timeLevel


      if (present(timeLevel)) then
         local_timeLevel = timeLevel
      else
         local_timeLevel = 1
      end if

      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)

      nullify(field%array)
      if (associated(mem)) then

         if (mem % Type /= WRFJEDI_POOL_INTEGER) then
            call pool_mesg('Error: Field '//trim(key)//' is not type integer.')
         end if
         if (mem % Ndim /= 3) then
            call pool_mesg('Error: Field '//trim(key)//' is not a 3-d field.')
         end if

         call field%fillFieldHead(mem)
         field%array => mem % ifield_3d

      else

         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')

      end if

   end subroutine wrfjedi_pool_get_field_3d_int!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_field_0d_char
!!
!!> \brief WRFJEDI Pool 0D Character field get subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a pointer to the field associated with key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_field_0d_char(inPool, key, field, timeLevel)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      type (field0DChar), pointer :: field
!      integer, intent(in), optional :: timeLevel
!
!      type (fieldlist), pointer :: mem
!      integer :: local_timeLevel
!
!
!      if (present(timeLevel)) then
!         local_timeLevel = timeLevel
!      else
!         local_timeLevel = 1
!      end if
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)
!
!      nullify(field)
!      if (associated(mem)) then
!
!         if (mem % contentsType /= WRFJEDI_POOL_CHARACTER) then
!            call pool_mesg('Error: Field '//trim(key)//' is not type character.')
!         end if
!         if (mem % contentsDims /= 0) then
!            call pool_mesg('Error: Field '//trim(key)//' is not a 0-d field.')
!         end if
!         if ((mem % contentsTimeLevs > 1) .and. (.not. present(timeLevel))) then
!            call pool_mesg('Error: Field '//trim(key)//' has more than one time level, but no timeLevel argument given.')
!         end if
!         if (mem % contentsTimeLevs < local_timeLevel) then
!            call pool_mesg('Error: Field '//trim(key)//' has too few time levels.')
!         end if
!         
!         if (mem % contentsTimeLevs == 1) then
!            field => mem % c0
!         else
!            field => mem % c0a(local_timeLevel)
!         end if
!
!      else
!
!         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')
!
!      end if
!
!   end subroutine wrfjedi_pool_get_field_0d_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_field_1d_char
!!
!!> \brief WRFJEDI Pool 1D Character field get subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a pointer to the field associated with key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_field_1d_char(inPool, key, field, timeLevel)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      type (field1DChar), pointer :: field
!      integer, intent(in), optional :: timeLevel
!
!      type (fieldlist), pointer :: mem
!      integer :: local_timeLevel
!
!
!      if (present(timeLevel)) then
!         local_timeLevel = timeLevel
!      else
!         local_timeLevel = 1
!      end if
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_FIELD)
!
!      nullify(field)
!      if (associated(mem)) then
!
!         if (mem % contentsType /= WRFJEDI_POOL_CHARACTER) then
!            call pool_mesg('Error: Field '//trim(key)//' is not type character.')
!         end if
!         if (mem % contentsDims /= 1) then
!            call pool_mesg('Error: Field '//trim(key)//' is not a 1-d field.')
!         end if
!         if ((mem % contentsTimeLevs > 1) .and. (.not. present(timeLevel))) then
!            call pool_mesg('Error: Field '//trim(key)//' has more than one time level, but no timeLevel argument given.')
!         end if
!         if (mem % contentsTimeLevs < local_timeLevel) then
!            call pool_mesg('Error: Field '//trim(key)//' has too few time levels.')
!         end if
!         
!         if (mem % contentsTimeLevs == 1) then
!            field => mem % c1
!         else
!            field => mem % c1a(local_timeLevel)
!         end if
!
!      else
!
!         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')
!
!      end if
!
!   end subroutine wrfjedi_pool_get_field_1d_char!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_0d_real
!
!> \brief WRFJEDI Pool 0D Real field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_0d_real(inPool, key, scalar, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      real (kind=RKIND), pointer :: scalar
      integer, intent(in), optional :: timeLevel

      type (field0DReal), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_0d_real(inPool, key, field, timeLevel)

      nullify(scalar)
      if (associated(field)) scalar => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_0d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_1d_real
!
!> \brief WRFJEDI Pool 1D Real field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_1d_real(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      real (kind=RKIND), dimension(:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field1DReal), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_1d_real(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_1d_real!}}}

!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_2d_real
!
!> \brief WRFJEDI Pool 2D Real field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_2d_real(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      real (kind=RKIND), dimension(:,:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field2DReal), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_2d_real(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_2d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_3d_real
!
!> \brief WRFJEDI Pool 3D Real field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_3d_real(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      real (kind=RKIND), dimension(:,:,:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field3DReal), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_3d_real(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
!      call field%printFieldHead()
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_3d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_4d_real
!
!> \brief WRFJEDI Pool 4D Real field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_4d_real(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      real (kind=RKIND), dimension(:,:,:,:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field4DReal), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_4d_real(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_4d_real!}}}


!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_array_5d_real
!!
!!> \brief WRFJEDI Pool 5D Real field get subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a pointer to the array associated with key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_array_5d_real(inPool, key, array, timeLevel)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      real (kind=RKIND), dimension(:,:,:,:,:), pointer :: array
!      integer, intent(in), optional :: timeLevel
!
!      type (field5DReal), pointer :: field
!
!
!      call wrfjedi_pool_get_field_5d_real(inPool, key, field, timeLevel)
!
!      nullify(array)
!      if (associated(field)) array => field % array
!
!   end subroutine wrfjedi_pool_get_array_5d_real!}}}
!
!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_0d_int
!
!> \brief WRFJEDI Pool 0D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_0d_int(inPool, key, scalar, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      integer, pointer :: scalar
      integer, intent(in), optional :: timeLevel

      type (field0DInteger), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_0d_int(inPool, key, field, timeLevel)

      nullify(scalar)
      if (associated(field)) scalar => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_0d_int!}}}

!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_1d_int
!
!> \brief WRFJEDI Pool 1D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_1d_int(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      integer, dimension(:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field1DInteger), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_1d_int(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_1d_int!}}}

!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_2d_int
!
!> \brief WRFJEDI Pool 2D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_2d_int(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      integer, dimension(:,:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field2DInteger), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_2d_int(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_2d_int!}}}

!
!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_array_3d_int
!
!> \brief WRFJEDI Pool 3D Integer field get subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This subroutine returns a pointer to the array associated with key in inPool.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_get_array_3d_int(inPool, key, array, timeLevel)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      integer, dimension(:,:,:), pointer :: array
      integer, intent(in), optional :: timeLevel

      type (field3DInteger), pointer :: field


      allocate(field)
      call wrfjedi_pool_get_field_3d_int(inPool, key, field, timeLevel)

      nullify(array)
      if (associated(field)) array => field % array
      deallocate(field)

   end subroutine wrfjedi_pool_get_array_3d_int!}}}


!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_array_0d_char
!!
!!> \brief WRFJEDI Pool 0D Character field get subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a pointer to the array associated with key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_array_0d_char(inPool, key, string, timeLevel)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      character (len=StrKIND), pointer :: string
!      integer, intent(in), optional :: timeLevel
!
!      type (field0DChar), pointer :: field
!
!
!      call wrfjedi_pool_get_field_0d_char(inPool, key, field, timeLevel)
!
!      nullify(string)
!      if (associated(field)) string => field % scalar
!
!   end subroutine wrfjedi_pool_get_array_0d_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_array_1d_char
!!
!!> \brief WRFJEDI Pool 1D Character field get subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a pointer to the array associated with key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_array_1d_char(inPool, key, array, timeLevel)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      character (len=StrKIND), dimension(:), pointer :: array
!      integer, intent(in), optional :: timeLevel
!
!      type (field1DChar), pointer :: field
!
!
!      call wrfjedi_pool_get_field_1d_char(inPool, key, field, timeLevel)
!
!      nullify(array)
!      if (associated(field)) array => field % array
!
!   end subroutine wrfjedi_pool_get_array_1d_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_config_real
!!
!!> \brief WRFJEDI Pool Real Config Insertion Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a real value as a config option into inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_config_real(inPool, key, value)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      real (kind=RKIND), intent(in) :: value
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_CONFIG
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_REAL
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_real)
!         newmem % data % simple_real = value
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_config_real!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_config_int
!!
!!> \brief WRFJEDI Pool Integer Config Insertion Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a integer value as a config option into inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_config_int(inPool, key, value)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      integer, intent(in) :: value
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_CONFIG
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_int)
!         newmem % data % simple_int = value
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_config_int!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_config_char
!!
!!> \brief WRFJEDI Pool Character Config Insertion Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a character string as a config option into inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_config_char(inPool, key, value)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      character (len=*), intent(in) :: value
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: oldLevel
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         oldLevel = currentErrorLevel
!
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_CONFIG
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_CHARACTER
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_char)
!         if (len_trim(value) > StrKIND) then
!            call wrfjedi_pool_set_error_level(WRFJEDI_POOL_WARN)
!            call pool_mesg('WARNING wrfjedi_pool_add_config_char: Input value for key '//trim(key)//' longer than StrKIND.')
!            call wrfjedi_pool_set_error_level(oldLevel)
!         end if
!         newmem % data % simple_char = value
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_config_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_config_logical
!!
!!> \brief WRFJEDI Pool Logical Config Insertion Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a logical flag as a config option into inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_config_logical(inPool, key, value)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      logical, intent(in) :: value
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % keyLen = len_trim(key)
!         newmem % key = trim(key)
!         newmem % contentsType = WRFJEDI_POOL_CONFIG
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_LOGICAL
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_logical)
!         newmem % data % simple_logical = value
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_config_logical!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_config_type
!!
!!> \brief Returns the type of a config in an WRFJEDI pool.
!!> \author Michael Duda
!!> \date   29 October 2014
!!> \details
!!> Returns the type of the specified config in the WRFJEDI pool. If the 
!!> config does not exist in the pool, a value of WRFJEDI_POOL_FATAL is returned.
!!
!!-----------------------------------------------------------------------
!   integer function wrfjedi_pool_config_type(inPool, key)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!
!      type (fieldlist), pointer :: mem
!
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_CONFIG)
!
!      if (associated(mem)) then
!         wrfjedi_pool_config_type = mem % contentsType
!      else
!         wrfjedi_pool_config_type = WRFJEDI_POOL_FATAL
!      end if
!
!   end function wrfjedi_pool_config_type!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_config_real
!!
!!> \brief WRFJEDI Pool Real Config Access Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns the value associated with a config option with the
!!> name key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_config_real(inPool, key, value, record)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      real (kind=RKIND), pointer :: value
!      character (len=*), intent(in), optional :: record
!      type (wrfjedi_pool_type), pointer :: recordPool
!
!      type (fieldlist), pointer :: mem
!
!      if ( present(record) ) then
!         call wrfjedi_pool_get_subpool(inPool, record, recordPool)
!         mem => pool_get_member(recordPool, key, WRFJEDI_POOL_CONFIG)
!      else
!         mem => pool_get_member(inPool, key, WRFJEDI_POOL_CONFIG)
!      end if
!
!      if (associated(mem)) then
!         if (mem % contentsType /= WRFJEDI_POOL_REAL) then
!            call pool_mesg('Error: Config '//trim(key)//' is not type real.')
!         end if
!         value => mem % simple_real
!      else
!         call pool_mesg('Error: Config '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_config_real!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_config_int
!!
!!> \brief WRFJEDI Pool Integer Config Access Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns the value associated with a config option with the
!!> name key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_config_int(inPool, key, value, record)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      integer, pointer :: value
!      character (len=*), intent(in), optional :: record
!      type (wrfjedi_pool_type), pointer :: recordPool
!
!      type (fieldlist), pointer :: mem
!
!      if ( present(record) ) then
!         call wrfjedi_pool_get_subpool(inPool, record, recordPool)
!         mem => pool_get_member(recordPool, key, WRFJEDI_POOL_CONFIG)
!      else
!         mem => pool_get_member(inPool, key, WRFJEDI_POOL_CONFIG)
!      end if
!
!      if (associated(mem)) then
!         if (mem % contentsType /= WRFJEDI_POOL_INTEGER) then
!            call pool_mesg('Error: Config '//trim(key)//' is not type integer.')
!         end if
!         value => mem % simple_int
!      else
!         call pool_mesg('Error: Config '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_config_int!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_config_char
!!
!!> \brief WRFJEDI Pool Character Config Access Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns the value associated with a config option with the
!!> name key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_config_char(inPool, key, value, record)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      character (len=StrKIND), pointer :: value
!      character (len=*), intent(in), optional :: record
!      type (wrfjedi_pool_type), pointer :: recordPool
!
!      type (fieldlist), pointer :: mem
!
!      if ( present(record) ) then
!         call wrfjedi_pool_get_subpool(inPool, record, recordPool)
!         mem => pool_get_member(recordPool, key, WRFJEDI_POOL_CONFIG)
!      else
!         mem => pool_get_member(inPool, key, WRFJEDI_POOL_CONFIG)
!      end if
!
!      if (associated(mem)) then
!         if (mem % contentsType /= WRFJEDI_POOL_CHARACTER) then
!            call pool_mesg('Error: Config '//trim(key)//' is not type character.')
!         end if
!
!         value => mem % simple_char
!      else
!         call pool_mesg('Error: Config '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_config_char!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_config_logical
!!
!!> \brief WRFJEDI Pool Logical Config Access Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns the value associated with a config option with the
!!> name key in inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_config_logical(inPool, key, value, record)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      logical, pointer :: value
!      character (len=*), intent(in), optional :: record
!      type (wrfjedi_pool_type), pointer :: recordPool
!
!      type (fieldlist), pointer :: mem
!
!      if ( present(record) ) then
!         call wrfjedi_pool_get_subpool(inPool, record, recordPool)
!         mem => pool_get_member(recordPool, key, WRFJEDI_POOL_CONFIG)
!      else
!         mem => pool_get_member(inPool, key, WRFJEDI_POOL_CONFIG)
!      end if
!
!      if (associated(mem)) then
!         if (mem % contentsType /= WRFJEDI_POOL_LOGICAL) then
!            call pool_mesg('Error: Config '//trim(key)//' is not type logical.')
!         end if
!         value => mem % simple_logical
!      else
!         call pool_mesg('Error: Config '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_config_logical!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_dimension_0d
!!
!!> \brief WRFJEDI Pool 0D Dimension Insertion routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a 0D dimension into inPool, and associated it with key.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_dimension_0d(inPool, key, dim)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      integer, intent(in) :: dim
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_DIMENSION
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_int)
!         newmem % data % simple_int = dim
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_dimension_0d!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_dimension_1d
!!
!!> \brief WRFJEDI Pool 1D Dimension Insertion routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a 1D dimension into inPool, and associated it with key.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_dimension_1d(inPool, key, dims)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      integer, dimension(:), intent(in) :: dims
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_DIMENSION
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_INTEGER
!         newmem % data % contentsDims = 1
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_int_arr(size(dims)))
!         newmem % data % simple_int_arr(:) = dims(:)
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data % simple_int_arr)
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_dimension_1d!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_dimension_0d
!!
!!> \brief WRFJEDI Pool 0D Dimension Access subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns the value of the 0D dimension associated with key in
!!>  inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_dimension_0d(inPool, key, dim)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      integer, pointer :: dim
!
!      type (fieldlist), pointer :: mem
!
!      nullify(dim)
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_DIMENSION)
!
!      if (associated(mem)) then
!         if (mem % contentsDims /= 0) then
!            call pool_mesg('Error: Dimension '//trim(key)//' is not a scalar.')
!         else
!            dim => mem % simple_int
!         end if
!      else
!         call pool_mesg('Error: Dimension '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_dimension_0d!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_dimension_1d
!!
!!> \brief WRFJEDI Pool 1D Dimension Access subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns the value of the 1D dimension associated with key in
!!>  inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_dimension_1d(inPool, key, dims)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      integer, pointer, dimension(:) :: dims
!
!      type (fieldlist), pointer :: mem
!
!      nullify(dims)
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_DIMENSION)
!
!      if (associated(mem)) then
!         if (mem % contentsDims /= 1) then
!            call pool_mesg('Error: Dimension '//trim(key)//' is not an array.')
!         else
!            dims => mem % simple_int_arr
!         end if
!      else
!         call pool_mesg('Error: Dimension '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_dimension_1d!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_add_subpool
!!
!!> \brief WRFJEDI Pool Subpool insertion routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine inserts a subpool (subPool) into inPool and associated it with
!!>  the name key.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_subpool(inPool, key, subPool)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      type (wrfjedi_pool_type), intent(in), target :: subPool
!
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_SUBPOOL
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_SUBPOOL
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         newmem % data % p => subPool
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_subpool!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_subpool
!!
!!> \brief WRFJEDI Pool Subpool access subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine returns a pointer to the subpool named key within inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_subpool(inPool, key, subPool)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      type (wrfjedi_pool_type), pointer :: subPool
!
!      type (fieldlist), pointer :: mem
!
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_SUBPOOL)
!
!      if (associated(mem)) then
!         subPool => mem % p
!      else
!         call pool_mesg('Error: Sub-pool '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_subpool!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_add_package
!!
!!> \brief WRFJEDI Pool Package insertion subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine inserts a package into a inPool and associates it with the
!!>  name key.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_add_package(inPool, key, value)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!      logical, intent(in) :: value
!
!      type (wrfjedi_pool_member_type), pointer :: newmem
!      integer :: threadNum
!
!      threadNum = wrfjedi_threading_get_thread_num()
!
!      if ( threadNum == 0 ) then
!         allocate(newmem)
!         newmem % key = trim(key)
!         newmem % keyLen = len_trim(key)
!         newmem % contentsType = WRFJEDI_POOL_PACKAGE
!
!         allocate(newmem % data)
!         newmem % data % contentsType = WRFJEDI_POOL_LOGICAL
!         newmem % data % contentsDims = 0
!         newmem % data % contentsTimeLevs = 0
!         allocate(newmem % data % simple_logical)
!         newmem % data % simple_logical = value
!   
!         if (.not. pool_add_member(inPool, key, newmem)) then
!            deallocate(newmem % data)
!            deallocate(newmem)
!         end if
!      end if
!
!   end subroutine wrfjedi_pool_add_package!}}}
!
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_get_package
!!
!!> \brief WRFJEDI Pool Package access subroutine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This subroutine sets the package pointer to point to the logical associated
!!>  with the package in inPool with name key.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_get_package(inPool, key, package)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(in) :: inPool
!      character (len=*), intent(in) :: key
!      logical, pointer :: package
!
!      type (fieldlist), pointer :: mem
!
!
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_PACKAGE)
!
!      if (associated(mem)) then
!         package => mem % simple_logical
!      else
!         call pool_mesg('Error: Package '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_get_package!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_remove_field
!!
!!> \brief WRFJEDI Pool Field Removal Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine removes a field with the name key from inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_remove_field(inPool, key)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!
!
!      if (.not. pool_remove_member(inPool, key, WRFJEDI_POOL_FIELD)) then
!         call pool_mesg('Error: Field '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_remove_field!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_remove_config
!!
!!> \brief WRFJEDI Pool Config Removal Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine removes a config with the name key from inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_remove_config(inPool, key)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!
!      type (fieldlist), pointer :: mem
!
!      !todo: if configs are pointers when being added, don't deallocate when removing.
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_CONFIG)
!
!      if (.not. associated(mem)) then
!         call pool_mesg('Error: Config '//trim(key)//' not found in pool.')
!         return
!      end if
!
!      if (mem % contentsType == WRFJEDI_POOL_REAL) then
!         deallocate(mem % simple_real)
!      else if (mem % contentsType == WRFJEDI_POOL_INTEGER) then
!         deallocate(mem % simple_int)
!      else if (mem % contentsType == WRFJEDI_POOL_CHARACTER) then
!         deallocate(mem % simple_char)
!      else if (mem % contentsType == WRFJEDI_POOL_LOGICAL) then
!         deallocate(mem % simple_logical)
!      end if
!
!      if (.not. pool_remove_member(inPool, key, WRFJEDI_POOL_CONFIG)) then
!         call pool_mesg('Error: Config '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_remove_config!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_remove_dimension
!!
!!> \brief WRFJEDI Pool Dimension Removal Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine removes a dimension with the name key from inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_remove_dimension(inPool, key)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!
!      type (fieldlist), pointer :: mem
!
!      !todo: if dimensions are pointers when being added, don't deallocate when removing.
!      mem => pool_get_member(inPool, key, WRFJEDI_POOL_DIMENSION)
!
!      if (.not. associated(mem)) then
!         call pool_mesg('Error: Dimension '//trim(key)//' not found in pool.')
!         return
!      end if
!
!      if (mem % contentsDims == 0) then
!         deallocate(mem % simple_int)
!      else if (mem % contentsDims == 1) then
!         deallocate(mem % simple_int_arr)
!      end if
!
!      if (.not. pool_remove_member(inPool, key, WRFJEDI_POOL_DIMENSION)) then
!         call pool_mesg('Error: Dimension '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_remove_dimension!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_remove_subpool
!!
!!> \brief WRFJEDI Pool Subpool Removal Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine removes a subpool with the name key from inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_remove_subpool(inPool, key)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!
!
!      if (.not. pool_remove_member(inPool, key, WRFJEDI_POOL_SUBPOOL)) then
!         call pool_mesg('Error: Sub-pool '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_remove_subpool!}}}
!
!
!!-----------------------------------------------------------------------
!!  routine wrfjedi_pool_remove_package
!!
!!> \brief WRFJEDI Pool Package Removal Routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine removes a package with the name key from inPool.
!!
!!-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_remove_package(inPool, key)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!      character (len=*), intent(in) :: key
!
!
!      if (.not. pool_remove_member(inPool, key, WRFJEDI_POOL_PACKAGE)) then
!         call pool_mesg('Error: Package '//trim(key)//' not found in pool.')
!      end if
!
!   end subroutine wrfjedi_pool_remove_package!}}}
!
!
!-----------------------------------------------------------------------
!  routine wrfjedi_pool_begin_iteration
!
!> \brief WRFJEDI Pool Begin Iteration Routine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This routine sets up the pool's internal iterator to iterate over fields.
!
!-----------------------------------------------------------------------
   subroutine wrfjedi_pool_begin_iteration(inPool)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool

      integer :: i, threadNum

      threadNum = wrfjedi_threading_get_thread_num()

      if ( threadNum == 0 ) then
         inPool % iterator => inPool % iteration_head
      end if

      !$omp barrier

   end subroutine wrfjedi_pool_begin_iteration!}}}


!-----------------------------------------------------------------------
!  subroutine wrfjedi_pool_get_next_member
!
!> \brief WRFJEDI Pool Iterate To Next Member subroutine
!> \author Ming Hu
!> \date   03/27/2014
!> \details
!> This function advances the internal iterator to the next member in the pool,
!>  and returns an iterator type for the current member, if one exists. The function
!>  returns .true. if a valid member was returned, and .false. if there are no members
!>  left to be iterated over.
!
!-----------------------------------------------------------------------
   logical function wrfjedi_pool_get_next_member(inPool, iterator)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(inout) :: inPool
      type (wrfjedi_pool_iterator_type),  intent(inout) :: iterator

      integer :: i, threadNum

      threadNum = wrfjedi_threading_get_thread_num()
      !$omp barrier

      !
      ! As long as there are members left to be iterated over, the inPool%iterator
      !   should always be pointing to the next member to be returned
      !
      if (associated(inPool % iterator)) then
         iterator % memberName = inPool % iterator % key
         iterator % memberType = inPool % iterator % contentsType
         iterator % dataType = inPool % iterator % data % Type
         if (iterator % memberType == WRFJEDI_POOL_FIELD) then
            iterator % nDims = inPool % iterator % data % Ndim
            iterator % nTimeLevels = 1
         else if (iterator % memberType == WRFJEDI_POOL_DIMENSION) then
            iterator % nDims = inPool % iterator % data % Ndim
         else
            iterator % nDims = 0
            iterator % nTimeLevels = 0
         end if
         wrfjedi_pool_get_next_member = .true.
      else
         wrfjedi_pool_get_next_member = .false.
      end if

      !$omp barrier

      if ( threadNum == 0 .and. associated(inPool % iterator) ) then
         ! Only thread 0 can advance iterator to next item
         inPool % iterator => inPool % iterator % iteration_next
      end if

   end function wrfjedi_pool_get_next_member!}}}


!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_shift_time_levels
!!
!!> \brief WRFJEDI Pool Time level shift routine
!!> \author Ming Hu
!!> \date   03/27/2014
!!> \details
!!> This routine shifts the time levels of all multi-level fields contained within.
!!>   When shifting, time level 1 becomes time level n, and time level i becomes time level i-1.
!!
!!-----------------------------------------------------------------------
!   recursive subroutine wrfjedi_pool_shift_time_levels(inPool)!{{{
!
!      implicit none
!
!      type (wrfjedi_pool_type), intent(inout) :: inPool
!
!
!      integer :: i, j
!      type (wrfjedi_pool_member_type), pointer :: ptr
!      type (fieldlist), pointer :: dptr
!
!
!      do i=1,inPool % size
!
!         ptr => inPool % table(i) % head
!         do while(associated(ptr))
!
!            if (ptr % contentsType == WRFJEDI_POOL_FIELD) then
!
!               dptr => ptr % data
!
!               if (associated(dptr % r0a)) then
!                  call wrfjedi_shift_time_levs(dptr % r0a)
!               else if (associated(dptr % r1a)) then
!                  call wrfjedi_shift_time_levs(dptr % r1a)
!               else if (associated(dptr % r2a)) then
!                  call wrfjedi_shift_time_levs(dptr % r2a)
!               else if (associated(dptr % r3a)) then
!                  call wrfjedi_shift_time_levs(dptr % r3a)
!               else if (associated(dptr % r4a)) then
!                  call wrfjedi_shift_time_levs(dptr % r4a)
!               else if (associated(dptr % r5a)) then
!                  call wrfjedi_shift_time_levs(dptr % r5a)
!               else if (associated(dptr % i0a)) then
!                  call wrfjedi_shift_time_levs(dptr % i0a)
!               else if (associated(dptr % i1a)) then
!                  call wrfjedi_shift_time_levs(dptr % i1a)
!               else if (associated(dptr % i2a)) then
!                  call wrfjedi_shift_time_levs(dptr % i2a)
!               else if (associated(dptr % i3a)) then
!                  call wrfjedi_shift_time_levs(dptr % i3a)
!               else if (associated(dptr % c0a)) then
!                  call wrfjedi_shift_time_levs(dptr % c0a)
!               else if (associated(dptr % c1a)) then
!                  call wrfjedi_shift_time_levs(dptr % c1a)
!               else if (associated(dptr % l0a)) then
!                  call wrfjedi_shift_time_levs(dptr % l0a)
!               end if
!
!            else if (ptr % contentsType == WRFJEDI_POOL_SUBPOOL) then
!
!               call wrfjedi_pool_shift_time_levels(ptr % data % p)
!
!            end if
!
!            ptr => ptr % next
!         end do
!
!      end do
!
!   end subroutine wrfjedi_pool_shift_time_levels!}}}
!
!!-----------------------------------------------------------------------
!!  subroutine wrfjedi_pool_print_summary
!!
!!> \brief WRFJEDI Pool Summary write routine
!!> \author Doug Jacobsen
!!> \date   07/29/2015
!!> \details
!!> This routine writes out a summary of the contents of a pool that match a specific type.
!!> The memberType input argument can take the value of WRFJEDI_POOL_FIELD,
!!> WRFJEDI_POOL_CONFIG, or WRFJEDI_POOL_PACKAGE.
!!> The recurseSubpools_in input argument defaults to false, but can be used to
!!> write a summary of subpool contents in addition to the current pool.
!!>
!!> It's important to note that this routine iterates over a pool. So if it is
!!> called within a pool iteration loop, it could cause issues.
!!
!!-----------------------------------------------------------------------
!   recursive subroutine wrfjedi_pool_print_summary(inPool, memberType, recurseSubpools_in)!{{{
!      type (wrfjedi_pool_type), pointer :: inPool
!      integer, intent(in) :: memberType
!      logical, optional, intent(in) :: recurseSubpools_in
!
!      type (wrfjedi_pool_iterator_type) :: poolItr
!      logical :: recurseSubpools
!
!      type (wrfjedi_pool_type), pointer :: subPool
!      real (kind=RKIND), pointer :: tempReal
!      integer, pointer :: tempInteger
!      logical, pointer :: tempLogical
!      character (len=StrKIND), pointer :: tempChar
!
!      if ( present(recurseSubpools_in) ) then
!         recurseSubpools = recurseSubpools_in
!      else
!         recurseSubpools = .false.
!      end if
!
!      call wrfjedi_pool_begin_iteration(inPool)
!      do while ( wrfjedi_pool_get_next_member(inPool, poolItr) )
!         ! Handle writing out configs
!         if ( poolItr % memberType == memberType .and. memberType == WRFJEDI_POOL_CONFIG ) then
!            if ( poolItr % dataType == WRFJEDI_POOL_REAL ) then
!               call wrfjedi_pool_get_config(inPool, poolItr % memberName, tempReal)
!               write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = ', tempReal
!            else if ( poolItr % dataType == WRFJEDI_POOL_INTEGER ) then
!               call wrfjedi_pool_get_config(inPool, poolItr % memberName, tempInteger)
!               write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = ', tempInteger
!            else if ( poolItr % dataType == WRFJEDI_POOL_LOGICAL ) then
!               call wrfjedi_pool_get_config(inPool, poolItr % memberName, tempLogical)
!               if ( tempLogical ) then
!                  write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = .true.'
!               else
!                  write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = .false.'
!               end if
!            else if ( poolItr % dataType == WRFJEDI_POOL_CHARACTER ) then
!               call wrfjedi_pool_get_config(inPool, poolItr % memberName, tempChar)
!               write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = ''' // trim(tempChar) // ''''
!            end if
!         ! Handle packages
!         else if (poolItr % memberType == memberType .and. memberType == WRFJEDI_POOL_PACKAGE ) then
!            call wrfjedi_pool_get_package(inPool, poolItr % memberName, tempLogical)
!            if ( tempLogical ) then
!               write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = .true.'
!            else
!               write(stderrUnit, *) '      ' // trim(poolItr % memberName) // ' = .false.'
!            end if
!         ! Handle fields
!         else if (poolItr % memberType == memberType .and. memberType == WRFJEDI_POOL_FIELD ) then
!            write(stderrUnit, *) '      ' // trim(poolItr % memberName)
!            if ( poolItr % dataType == WRFJEDI_POOL_REAL) then
!               write(stderrUnit, *) '            Type: Real'
!            else if ( poolItr % dataType == WRFJEDI_POOL_INTEGER) then
!               write(stderrUnit, *) '            Type: Integer'
!            else if ( poolItr % dataType == WRFJEDI_POOL_CHARACTER) then
!               write(stderrUnit, *) '            Type: Character'
!            else if ( poolItr % dataType == WRFJEDI_POOL_LOGICAL) then
!               write(stderrUnit, *) '            Type: Logical'
!            end if
!            write(stderrUnit, *) '            Number of dimensions: ', poolItr % nDims
!            write(stderrUnit, *) '            Number of time levels: ', poolItr % nTimeLevels
!         else if (poolItr % memberType == WRFJEDI_POOL_SUBPOOL .and. recurseSubpools ) then
!            write(stderrUnit, *) '   ** Begin subpool: ' // trim(poolItr % memberName)
!            call wrfjedi_pool_get_subpool(inPool, poolItr % memberName, subPool)
!            call wrfjedi_pool_print_summary(subPool, memberType, recurseSubpools)
!            write(stderrUnit, *) '   ** End subpool: ' // trim(poolItr % memberName)
!         end if
!      end do
!
!   end subroutine wrfjedi_pool_print_summary!}}}
!
!!!!!!!!!!! Private subroutines !!!!!!!!!!
!
   logical function pool_add_member(inPool, key, newmem)!{{{
!
      implicit none
!
      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      type (wrfjedi_pool_member_type), pointer :: newmem
!
      integer :: hash, oldLevel
      type (wrfjedi_pool_member_type), pointer :: ptr
!
      call pool_hash(hash, trim(newmem % key), newmem % keylen)
!
      hash = mod(hash, inPool % size) + 1
!
      pool_add_member = .true.
!
      if (.not. associated(inPool % table(hash) % head)) then
         inPool % table(hash) % head => newmem
      else
         ptr => inPool % table(hash) % head
         do while (associated(ptr % next))
!
            if (ptr % contentsType == newmem % contentsType .and. &
                ptr % keyLen == newmem % keyLen) then
               if (ptr % key(1:ptr%keyLen) == newmem % key(1:newmem%keyLen)) then
                  pool_add_member = .false.
                  call wrfjedi_pool_set_error_level(WRFJEDI_POOL_FATAL)
                  call pool_mesg('Error: Field '//trim(key)//' already exists in pool.')
                  return
               end if
            end if

            ptr => ptr % next
         end do
         ptr % next => newmem
      end if
!
!      !
!      ! Link the new member into the iteration list
!      !
      if (.not. associated(inPool % iteration_head)) then
         inPool % iteration_head => newmem
         inPool % iteration_tail => newmem
      else
         newmem % iteration_prev => inPool % iteration_tail
         inPool % iteration_tail => newmem
         newmem % iteration_prev % iteration_next => newmem
      end if
!
   end function pool_add_member!}}}
!
!
   function pool_get_member(inPool, key, memType)!{{{
!
      implicit none
!
      type (wrfjedi_pool_type), intent(in) :: inPool
      character (len=*), intent(in) :: key
      integer, intent(in) :: memType
!
      type (fieldlist), pointer :: pool_get_member
!
      integer :: hash, endl
      type (wrfjedi_pool_member_type), pointer :: ptr
!
!
      nullify(pool_get_member)
!
      endl = len_trim(key)
      call pool_hash(hash, key, endl)
!
      hash = mod(hash, inPool % size) + 1
!
      ptr => inPool % table(hash) % head
      do while (associated(ptr))
         if (ptr % contentsType == memType) then
            if (endl == ptr % keyLen) then
               if (key(1:endl) == ptr % key(1:endl)) then
                  pool_get_member => ptr % data
                  exit
               end if
            end if
         end if
         ptr => ptr % next
      end do
!
   end function pool_get_member!}}}
!
!
   logical function pool_remove_member(inPool, key, memType)!{{{
!
      implicit none
!
      type (wrfjedi_pool_type), intent(inout) :: inPool
      character (len=*), intent(in) :: key
      integer, intent(in) :: memType
!
      integer :: hash, endl
      type (wrfjedi_pool_member_type), pointer :: ptr, ptr_prev
      integer :: threadNum
!
      threadNum = wrfjedi_threading_get_thread_num()
!
      endl = len_trim(key)
      call pool_hash(hash, key, endl)
!
      hash = mod(hash, inPool % size) + 1
!
      if (associated(inPool % table(hash) % head)) then
!
         ! Is the member at the head of the list?
         ptr_prev => inPool % table(hash) % head
         if (ptr_prev % contentsType == memType) then
            if (endl == ptr_prev % keyLen) then
               if (key(1:endl) == ptr_prev % key(1:endl)) then
                  inPool % table(hash) % head => ptr_prev % next
                  if ( threadNum == 0 ) then

                     !
                     ! Un-link the member from the iteration list
                     !
                     if (associated(ptr_prev % iteration_prev)) then
                        ptr_prev % iteration_prev % iteration_next => ptr_prev % iteration_next
                     else
                        inPool % iteration_head => ptr_prev % iteration_next
                     end if
!
                     if (associated(ptr_prev % iteration_next)) then
                        ptr_prev % iteration_next % iteration_prev => ptr_prev % iteration_prev
                     else
                        inPool % iteration_tail => ptr_prev % iteration_prev
                     end if

!TODO: are there cases where we need to delete more data here?
                     deallocate(ptr_prev)
                  end if
                  pool_remove_member = .true.
                  return
               end if
            end if
         end if

         ! Possibly later in the list?
         ptr => ptr_prev % next
         do while (associated(ptr))
            if (ptr % contentsType == memType) then
               if (endl == ptr % keyLen) then
                  if (key(1:endl) == ptr % key(1:endl)) then
                     if ( threadNum == 0 ) then
                        ptr_prev % next => ptr % next

                        !
                        ! Un-link the member from the iteration list
                        !
                        if (associated(ptr % iteration_prev)) then
                           ptr % iteration_prev % iteration_next => ptr % iteration_next
                        else
                           inPool % iteration_head => ptr % iteration_next
                        end if
!   
                        if (associated(ptr % iteration_next)) then
                           ptr % iteration_next % iteration_prev => ptr % iteration_prev
                        else
                           inPool % iteration_tail => ptr % iteration_prev
                        end if

!TODO: are there cases where we need to delete more data here?
                        deallocate(ptr)
                     end if
                     pool_remove_member = .true.
                     return
                  end if
               end if
            end if
            ptr => ptr % next
            ptr_prev => ptr_prev % next
         end do

      end if

      pool_remove_member = .false.

   end function pool_remove_member!}}}
!
!
   subroutine pool_mesg(mesg)!{{{
!
      implicit none
!
      character (len=*), intent(in) :: mesg
      integer :: threadNum
      integer :: stderrUnit=6
!
      threadNum = wrfjedi_threading_get_thread_num()
!
      if (currentErrorLevel == WRFJEDI_POOL_WARN) then
         if ( threadNum == 0 ) then
            write(stderrUnit,*) trim(mesg)
         end if
      else if (currentErrorLevel == WRFJEDI_POOL_FATAL) then
         if ( threadNum == 0 ) then
            write(stderrUnit,*) trim(mesg)
         end if
!mhu         call wrfjedi_dmpar_global_abort(trim(mesg))
         stop 1234
      end if

   end subroutine pool_mesg!}}}
!
!
   subroutine pool_print_table_size(pool)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in) :: pool

      integer :: i, head_size, total_size
      type (wrfjedi_pool_member_type), pointer :: ptr


      total_size = 0
      do i=1,pool % size
         head_size = 0
         ptr => pool % table(i) % head
         do while (associated(ptr))
            head_size = head_size + 1
            ptr => ptr % next
         end do
         write(stderrUnit,*) 'List ', i, ' : ', head_size
         total_size = total_size + head_size
      end do
      write(stderrUnit,*) '----------------'
      write(stderrUnit,*) 'Total: ', total_size

   end subroutine pool_print_table_size!}}}


   recursive subroutine pool_print_members(pool, poolname)!{{{

      implicit none

      type (wrfjedi_pool_type), intent(in),pointer :: pool
      character(len=*),intent(in),optional :: poolname

      integer :: i
      type (wrfjedi_pool_type), pointer :: subpool
      type (wrfjedi_pool_member_type), pointer :: ptr
      type (wrfjedi_pool_iterator_type) :: poolItr
!
      real (kind=RKIND), pointer :: realPtr
      integer, pointer :: intPtr
      logical, pointer :: logPtr
      character (len=StrKIND) :: charPtr
      type (field0DReal), pointer :: field0d
      type (field1DReal), pointer :: field1d
      type (field2DReal), pointer :: field2d
      type (field3DReal), pointer :: field3d
      type (field4DReal), pointer :: field4d
      type (field0DInteger), pointer :: ifield0d
      type (field1DInteger), pointer :: ifield1d
      type (field2DInteger), pointer :: ifield2d
      type (field3DInteger), pointer :: ifield3d
      integer :: j

      write(stderrUnit, *) '~~~~~~~~~~~~~ pool_print_members from pool ~~~~~~~~~~~'
      if(present(poolname)) then
          write(stderrUnit, *) ' pool name is ===> ', trim(poolname)
      endif
      if(.not.associated(pool)) then
          write(stderrUnit, *) ' this pool pointer is not associated with any pool yet'
          return
      endif
      write(stderrUnit, *) '   Constants: '
      write(stderrUnit, *) '   Real: ', WRFJEDI_POOL_REAL
      write(stderrUnit, *) '   Integer: ', WRFJEDI_POOL_INTEGER
      write(stderrUnit, *) '   Logical: ', WRFJEDI_POOL_LOGICAL
      write(stderrUnit, *) '   Character: ', WRFJEDI_POOL_CHARACTER

!     write(stderrUnit, *) 'Pool Size:'
!     call pool_print_table_size(pool)
!
      call wrfjedi_pool_begin_iteration(pool)
      do while(wrfjedi_pool_get_next_member(pool, poolItr))

         if (poolItr % memberType == WRFJEDI_POOL_SUBPOOL) then
            write(stderrUnit, *) '** Found subpool named: ', trim(poolItr % memberName)
!            call wrfjedi_pool_get_subpool(pool, trim(poolItr % memberName), subpool)
!            call pool_print_members(subpool)
         else if (poolItr % memberType == WRFJEDI_POOL_CONFIG) then
            write(stderrUnit, *) '   Config Option: ', trim(poolItr % memberName), poolItr % dataType
         else if (poolItr % memberType == WRFJEDI_POOL_DIMENSION) then
            write(stderrUnit, *) '   Dimension: ', trim(poolItr % memberName), poolItr % dataType, poolItr % nDims
         else if (poolItr % memberType == WRFJEDI_POOL_PACKAGE) then
            write(stderrUnit, *) '   Package: ', trim(poolItr % memberName)
         else if (poolItr % memberType == WRFJEDI_POOL_FIELD) then
            write(stderrUnit, *) 'pool_print_members===> Field: ', trim(poolItr % memberName), ' ',&
                               poolItr % dataType, poolItr % nDims,poolItr % nTimeLevels

            if (poolItr % dataType == WRFJEDI_POOL_REAL) then
               if (poolItr % nDims == 0) then
                   allocate(field0d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), field0d)
                   write(*,*) 'pool_print_members===>  0D real value: ', &
                                  field0d % array
                   call field0d%printFieldHead()
                   deallocate(field0d)
               else if (poolItr % nDims == 1) then
                   allocate(field1d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), field1d)
                   write(*,*) 'pool_print_members===>  1D real MIN/MAX value: ', &
                                  minval(field1d % array),maxval(field1d % array)
                   call field1d%printFieldHead()
                   deallocate(field1d)
               else if (poolItr % nDims == 2) then
                   allocate(field2d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), field2d)
                   write(*,*) 'pool_print_members===>  2D real MIN/MAX value: ', &
                                  minval(field2d % array),maxval(field2d % array)
!                   do j=field2d%sm2,field2d%sd2
!                      write(*,*) j,field2d % array(field2d%sm1:field2d%sd1,j)
!                   enddo
!                   do j=field2d%ed2,field2d%em2
!                      write(*,*) j,field2d % array(field2d%ed1:field2d%em1,j)
!                   enddo
                   call field2d%printFieldHead()
                   deallocate(field2d)
               else if (poolItr % nDims == 3) then
                   allocate(field3d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), field3d)
                   write(*,*) 'pool_print_members===>  3D real MIN/MAX value: ', &
                                  minval(field3d % array),maxval(field3d % array)
                   call field3d%printFieldHead()
                   deallocate(field3d)
               else if (poolItr % nDims == 4) then
                   allocate(field4d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), field4d)
                   write(*,*) 'pool_print_members===>  4D real MIN/MAX value: ', &
                                  minval(field4d % array),maxval(field4d % array)
                   call field4d%printFieldHead()
                   deallocate(field4d)
               endif
            endif

            if (poolItr % dataType == WRFJEDI_POOL_INTEGER) then
               if (poolItr % nDims == 0) then
                   allocate(ifield0d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), ifield0d)
                   write(*,*) 'pool_print_members===>  0D integer value: ', &
                                  ifield0d % array
                   deallocate(ifield0d)
               else if (poolItr % nDims == 1) then
                   allocate(ifield1d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), ifield1d)
                   write(*,*) 'pool_print_members===>  1D integer  MIN/MAX value: ', &
                                  minval(ifield1d % array),maxval(ifield1d % array)
                   call ifield1d%printFieldHead()
                   deallocate(ifield1d)
               else if (poolItr % nDims == 2) then
                   allocate(ifield2d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), ifield2d)
                   write(*,*) 'pool_print_members===>  2D integer  MIN/MAX value: ', &
                                  minval(ifield2d % array),maxval(ifield2d % array)
                   call ifield2d%printFieldHead()
                   deallocate(ifield2d)
               else if (poolItr % nDims == 3) then
                   allocate(ifield3d)
                   call wrfjedi_pool_get_field(pool, trim(poolItr % memberName), ifield3d)
                   write(*,*) 'pool_print_members===>  3D integer  MIN/MAX value: ', &
                                  minval(ifield3d % array),maxval(ifield3d % array)
                   call ifield3d%printFieldHead()
                   deallocate(ifield3d)
               endif
            endif

         end if
      end do
      write(stderrUnit, *) '^^^^^^^^^^^^^ Done with print member pool ^^^^^^^^^^^^^^^^^'
      write(stderrUnit, *) ''

   end subroutine pool_print_members!}}}


!   integer function pool_get_member_decomp_type(dimName) result(decompType)!{{{
!      character (len=*) :: dimName
!
!
!      decompType = WRFJEDI_DECOMP_NONDECOMP
!
!      if (trim(dimName) == 'nCells') then
!         decompType = WRFJEDI_DECOMP_CELLS
!      else if (trim(dimName) == 'nEdges') then
!         decompType = WRFJEDI_DECOMP_EDGES
!      else if (trim(dimName) == 'nVertices') then
!         decompType = WRFJEDI_DECOMP_VERTICES
!      end if
!
!   end function pool_get_member_decomp_type!}}}
!
   function wrfjedi_threading_get_thread_num() result(threadNum)!{{{
      integer :: threadNum
      integer :: omp_get_thread_num

      threadNum = 0

!#ifdef MPAS_OPENMP
!      threadNum = omp_get_thread_num()
!#endif

   end function wrfjedi_threading_get_thread_num!}}}

!
   subroutine wrfjedi_duplicate_fieldlist(infield, newfield)

      type (fieldlist), intent(in), pointer :: infield
      type (fieldlist), intent(inout), pointer :: newfield
!
      type (field2DReal), pointer :: field
!
      integer :: sm1,em1,sm2,em2,sm3,em3
!
      integer :: ierr

      if (associated(infield)) then
   
!         write(*,*) '=======check duplicate_fieldlist head:'
         allocate(field)
         call field%fillFieldHead(infield)
         call field%sendFieldHead(newfield)
         call wrfjedi_nullify_fieldlist(newfield)
!         call field%printFieldHead()
         sm1=newfield%sm1
         em1=newfield%em1
         sm2=newfield%sm2
         em2=newfield%em2
         sm3=newfield%sm3
         em3=newfield%em3
         deallocate(field)
!         write(*,*) '=======check duplicate_fieldlist head:',sm1,em1,sm2,em2,sm3,em3
         if(newfield%Type==WRFJEDI_POOL_REAL ) then
            if(newfield%Ndim == 0) then
               ALLOCATE(newfield % rfield_0d,STAT=ierr)
               if(ierr==0) then
                  newfield % rfield_0d = infield % rfield_0d
               endif

            elseif(newfield%Ndim == 1) then
               ALLOCATE(newfield % rfield_1d(sm1:em1),STAT=ierr)
               if(ierr==0) then
                  newfield % rfield_1d = infield % rfield_1d
               endif

            elseif(newfield%Ndim == 2) then
               ALLOCATE(newfield % rfield_2d(sm1:em1,sm2:em2),STAT=ierr)
               if(ierr==0) then
                  newfield % rfield_2d = infield % rfield_2d
               endif

            elseif(newfield%Ndim == 3) then
               ALLOCATE(newfield % rfield_3d(sm1:em1,sm2:em2,sm3:em3),STAT=ierr)
               if(ierr==0) then
                  newfield % rfield_3d = infield % rfield_3d
               endif

            elseif(newfield%Ndim == 4) then
               ALLOCATE(newfield % rfield_4d(sm1:em1,sm2:em2,sm3:em3,1),STAT=ierr)
               if(ierr==0) then
                  newfield % rfield_4d = infield % rfield_4d
               endif

            else
               call pool_mesg('Error: unkonw dimension in input field.')
               write(*,*) 'newfield%Ndim=',newfield%Ndim
            endif

         else if(newfield%Type==WRFJEDI_POOL_INTEGER ) then
            if(newfield%Ndim == 0) then
               ALLOCATE(newfield % ifield_0d,STAT=ierr)
               if(ierr==0) then
                  newfield % ifield_0d = infield % ifield_0d
               endif

            elseif(newfield%Ndim == 1) then
               ALLOCATE(newfield % ifield_1d(sm1:em1),STAT=ierr)
               if(ierr==0) then
                  newfield % ifield_1d = infield % ifield_1d
               endif

            elseif(newfield%Ndim == 2) then
               ALLOCATE(newfield % ifield_2d(sm1:em1,sm2:em2),STAT=ierr)
               if(ierr==0) then
                  newfield % ifield_2d = infield % ifield_2d
               endif

            elseif(newfield%Ndim == 3) then
               ALLOCATE(newfield % ifield_3d(sm1:em1,sm2:em2,sm3:em3),STAT=ierr)
               if(ierr==0) then
                  newfield % ifield_3d = infield % ifield_3d
               endif

            else
               call pool_mesg('Error: unkonw dimension in input field.')
               write(*,*) 'newfield%Ndim=',newfield%Ndim
            endif
         else
            call pool_mesg('Error: unkonw data type '//newfield%Type//' in input field.')
         endif
      else
         call pool_mesg('Error: input Field is not associated.')
      end if

   end subroutine wrfjedi_duplicate_fieldlist

   subroutine wrfjedi_nullify_fieldlist(infield)

      type (fieldlist), intent(inout), pointer :: infield

      nullify(infield%rfield_0d)
      nullify(infield%rfield_1d)
      nullify(infield%rfield_2d)
      nullify(infield%rfield_3d)
      nullify(infield%rfield_4d)
      nullify(infield%rfield_5d)
      nullify(infield%rfield_6d)
      nullify(infield%rfield_7d)

      nullify(infield%dfield_0d)
      nullify(infield%dfield_1d)
      nullify(infield%dfield_2d)
      nullify(infield%dfield_3d)
      nullify(infield%dfield_4d)
      nullify(infield%dfield_5d)
      nullify(infield%dfield_6d)
      nullify(infield%dfield_7d)

      nullify(infield%ifield_0d)
      nullify(infield%ifield_1d)
      nullify(infield%ifield_2d)
      nullify(infield%ifield_3d)
      nullify(infield%ifield_4d)
      nullify(infield%ifield_5d)
      nullify(infield%ifield_6d)
      nullify(infield%ifield_7d)

      nullify(infield%lfield_0d)
      nullify(infield%lfield_1d)
      nullify(infield%lfield_2d)

   end subroutine wrfjedi_nullify_fieldlist
!
   function wrfjedi_compare_fieldlist(fieldA,fieldB)
   
      logical :: wrfjedi_compare_fieldlist
      type (fieldlist), intent(in), pointer :: fieldA,fieldB

      integer :: iCountDiff
      logical :: lSameDim

      iCountDiff=0
      lSameDim=.false.

!      if(fieldA% /= fieldB%) iCountDiff = iCountDiff + 1
      wrfjedi_compare_fieldlist=lSameDim

   end function wrfjedi_compare_fieldlist

   subroutine wrfjedi_getdimension_fieldlist(infield)

      type (fieldlist), intent(in), pointer :: infield

   end subroutine wrfjedi_getdimension_fieldlist 
!
end module wrfjedi_pool_routines
