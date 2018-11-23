! Copyright (c) 2018, National Atmospheric for Atmospheric Research (NCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://wrfjedi-dev.github.com/license.html

module wrfjedi4da_mod

   !***********************************************************************
   !
   !  Module wrfjedi4da_mod to encapsulate operations needed for
   !  Data assimilation purpose.
   !  It can be used from /somewhere/WRFJEDI/src/operators 
   !  or from /somewhere/wrfjedi-bundle/wrfjedi/model (OOPS) 
   !> \author  Gael Descombes/Mickael Duda NCAR/MMM
   !> \date    January 2018
   !
   !-----------------------------------------------------------------------

!   use wrfjedi_constants
   use wrfjedi_derived_types, only: field0DReal,field0DInteger, &
                                    field1DReal,field1DInteger, &
                                    field2DReal,field2DInteger, &
                                    field3DReal,field3DInteger, &
                                    field4DReal
   use wrfjedi_pool_routines
   use wrfjedi_kinds, only : kind_real,RKIND
   use module_domain_type, only: domain,fieldlist
   use module_domain, only :  get_ijk_from_grid
   !use random_vectors_mod
 
   implicit none
   private

   public :: da_make_subpool_wrfjedi, &
             da_check_grid_content_wrfjedi, &
             da_zeros,da_self_mult,da_random,da_fldrms,&
             da_dot_product,da_operator,da_axpy,da_gpnorm

   contains

!   !***********************************************************************
!   !
!   !  subroutine wrfjedi_pool_demo
!   !
!   !> \brief   Demonstrate basic usage of WRFJEDI pools
!   !> \author  Michael Duda
!   !> \date    20 December 2017
!   !> \details
!   !>  This routine provides a simple demonstration of how to construct a new
!   !>  pool at runtime, add members (fields) to the pool, and to perform generic
!   !>  operations on that pool.
!   !
!   !-----------------------------------------------------------------------
!   subroutine wrfjedi_pool_demo(block)
!
!      implicit none
!
!      type (block_type), pointer :: block
!
!      type (wrfjedi_pool_type), pointer :: structs
!      type (wrfjedi_pool_type), pointer :: allFields
!      type (wrfjedi_pool_type), pointer :: da_state
!      type (wrfjedi_pool_type), pointer :: da_state_incr
!
!      type (field2DReal), pointer :: field
!
!      write(0,*) '****** Begin pool demo routine ******'
!
!      structs => block % structs
!      allFields => block % allFields
!
!      !
!      ! Create a new pool
!      !
!      call wrfjedi_pool_create_pool(da_state)
!
!      !
!      ! Get pointers to several fields from the allFields pool, and add
!      ! those fields to the da_state pool as well
!      !
!      call wrfjedi_pool_get_field(allFields, 'theta', field)
!      call wrfjedi_pool_add_field(da_state, 'theta', field)
!      write(0,*) 'Now, max value of theta is ', maxval(field % array),minval(field % array)
!      field % array(:,:) = 1.0
!      write(0,*)'Dimensions Field: ',field % dimSizes(:)
!
!      call wrfjedi_pool_get_field(allFields, 'rho', field)
!      call wrfjedi_pool_add_field(da_state, 'rho', field)
!      write(0,*) 'Now, max value of rho is ', maxval(field % array),minval(field % array)
!      field % array(:,:) = 1.0
!
!      !
!      ! Create another pool
!      !
!      call wrfjedi_pool_create_pool(da_state_incr)
!
!      !
!      ! Duplicate the members of da_state into da_state_incr, and do a deep
!      ! copy of the fields from da_state to da_state_incr
!      !
!      call wrfjedi_pool_clone_pool(da_state, da_state_incr)
!
!      !
!      ! Call example algebra routine to compute A = A + B for all fields in
!      ! the da_state and da_state_inc pools
!      !
!      call da_operator_addition(da_state, da_state_incr)
!
!      call wrfjedi_pool_get_field(da_state_incr, 'rho', field)
!      write(0,*) 'Now, max value of rho_incr is ', maxval(field % array)
!
!      call wrfjedi_pool_get_field(da_state, 'rho', field)
!      write(0,*) 'Now, max value of rho is ', maxval(field % array)
!
!      !
!      ! Before destroying a pool, we should remove any fields that are
!      ! still referenced by other active pools to avoid deallocating them
!      !
!      call wrfjedi_pool_empty_pool(da_state)
!
!      !
!      ! Destroy the now-empty da_state pool
!      !
!      call wrfjedi_pool_destroy_pool(da_state)
!
!      !
!      ! Destroy the da_state_incr pool, deallocating all of its
!      ! fields in the process (because this pool was not emptied)
!      !
!      call wrfjedi_pool_destroy_pool(da_state_incr)
!
!      write(0,*) '****** End pool demo routine ******'
!
!   end subroutine wrfjedi_pool_demo
!
   !***********************************************************************
   !
   !  subroutine da_operator_addition
   !
   !> \brief   Performs A = A + B for pools A and B
   !> \author  Michael Duda
   !> \date    20 December 2017
   !> \details
   !>  Given two pools, A and B, where the fields in B are a subset of
   !>  the fields in A, this routine adds the fields in B to fields in A
   !>  with the same name. When A and B contain identical fields, this
   !>  is equivalent to A = A + B.
   !
   !-----------------------------------------------------------------------
   subroutine da_operator_addition(pool_a, pool_b)

      implicit none

      type (wrfjedi_pool_type), pointer :: pool_a, pool_b

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a, r0d_ptr_b
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_b)

      do while ( wrfjedi_pool_get_next_member(pool_b, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r0d_ptr_b)
                  r0d_ptr_a = r0d_ptr_a + r0d_ptr_b
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r1d_ptr_b)
                  r1d_ptr_a = r1d_ptr_a + r1d_ptr_b
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r2d_ptr_b)
                  r2d_ptr_a = r2d_ptr_a + r2d_ptr_b
                  write(0,*)'Operator add MIN/MAX: ',minval(r2d_ptr_a),maxval(r2d_ptr_a)
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r3d_ptr_b)
                  r3d_ptr_a = r3d_ptr_a + r3d_ptr_b
               end if

            end if
         end if
      end do

   end subroutine da_operator_addition


   !***********************************************************************
   !
   !  subroutine da_copy_all2sub_fields
   !
   !> \brief   Performs a copy of pool B to a pool A
   !> \author  Ming Hu
   !> \date    21 November 2018
   !> \details
   !>  Given two pools, allfields and A, where the fields in A are a subset of
   !>  the fields in allfields, this routine copy the fields allfields to fields in A
   !>  with the same name.
   !
   !-----------------------------------------------------------------------
   subroutine da_copy_poolB2A_fields(pool_b, pool_a)

      implicit none

      type (wrfjedi_pool_type), pointer, intent(inout) :: pool_a
      type (wrfjedi_pool_type), pointer, intent(in)    :: pool_b

      type (wrfjedi_pool_iterator_type) :: poolItr_a, poolItr_b
      real (kind=RKIND), pointer :: r0d_ptr_a, r0d_ptr_b
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b


      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_b)

      do while ( wrfjedi_pool_get_next_member(pool_b, poolItr_b) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr_b % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
           if (poolItr_b % dataType == WRFJEDI_POOL_REAL) then

             call wrfjedi_pool_begin_iteration(pool_a)
             do while ( wrfjedi_pool_get_next_member(pool_a, poolItr_a) )

               if (( trim(poolItr_b % memberName)).eq.(trim(poolItr_a % memberName)) ) then

                  ! Depending on the dimensionality of the field, we need to set pointers of
                  ! the correct type
                  if (poolItr_b % nDims == 0) then
                     call wrfjedi_pool_get_array(pool_a, trim(poolItr_a % memberName), r0d_ptr_a)
                     call wrfjedi_pool_get_array(pool_b, trim(poolItr_b % memberName), r0d_ptr_b)
                     r0d_ptr_a = r0d_ptr_b
                  else if (poolItr_b % nDims == 1) then
                     call wrfjedi_pool_get_array(pool_a, trim(poolItr_a % memberName), r1d_ptr_a)
                     call wrfjedi_pool_get_array(pool_b, trim(poolItr_b % memberName), r1d_ptr_b)
                     r1d_ptr_a = r1d_ptr_b
                  else if (poolItr_b % nDims == 2) then
                     call wrfjedi_pool_get_array(pool_a, trim(poolItr_a % memberName), r2d_ptr_a)
                     call wrfjedi_pool_get_array(pool_b, trim(poolItr_b % memberName), r2d_ptr_b)
                     r2d_ptr_a = r2d_ptr_b
                  else if (poolItr_b % nDims == 3) then
                     call wrfjedi_pool_get_array(pool_a, trim(poolItr_a % memberName), r3d_ptr_a)
                     call wrfjedi_pool_get_array(pool_b, trim(poolItr_b % memberName), r3d_ptr_b)
                     r3d_ptr_a = r3d_ptr_b
                  end if

               else
                  write(*,*)'WARNING in Copy all2sub field; ',trim(poolItr_a % memberName), &
                            'not available from WRFJEDI'
               end if
             end do  ! loop through pool_a
           end if
         end if
      end do

   end subroutine da_copy_poolB2A_fields


   !***********************************************************************
   !
   !  subroutine da_check_grid_content_wrfjedi
   !
   !> \brief   list content from wrf grid
   !> \author  Ming Hu
   !> \date    20 November 2018
   !> \details
   !>  list content from a wrf grid
   !
   !-----------------------------------------------------------------------
   subroutine da_check_grid_content_wrfjedi(domain_grid)

      implicit none

      type (domain), pointer, intent(in) :: domain_grid

      integer :: ii
      INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe
      CHARACTER*80  dname, memord

      TYPE( fieldlist ), POINTER :: p
!
! get dimension for this domain
      call get_ijk_from_grid (  domain_grid ,                  &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

      p => domain_grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) )

        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN
          dname = p%DataName
          IF (p%Ntl.GT.0)  dname=dname(1:len(TRIM(dname))-2)
          write(*,'(I4,A5,I5,3A15,A5,A10,A20)') p%Ndim, p%Type, p%Ntl, &
                      TRIM(p%VarName),trim(p%DataName),trim(dname),&
                      p%ProcOrient,trim(p%MemoryOrder) !,trim(p%dimname2)
          if(p%Type == WRFJEDI_POOL_REAL )then
              if(p%Ndim==1) then
                 write(*,*) 'MAX/MIN==>',maxval(p%rfield_1d),minval(p%rfield_1d)
              elseif(p%Ndim==2) then
                 write(*,*) 'MAX/MIN==>',maxval(p%rfield_2d),minval(p%rfield_2d)
              elseif(p%Ndim==3) then
                 write(*,*) 'MAX/MIN==>',maxval(p%rfield_3d),minval(p%rfield_3d)
              elseif(p%Ndim==4) then
                 write(*,*) 'MAX/MIN==>',maxval(p%rfield_4d),minval(p%rfield_4d)
              else
              endif
          elseif(p%Type == WRFJEDI_POOL_INTEGER) then
              if(p%Ndim==1) then
                 write(*,*) 'MAX/MIN==>',maxval(p%ifield_1d),minval(p%ifield_1d)
              elseif(p%Ndim==2) then
                 write(*,*) 'MAX/MIN==>',maxval(p%ifield_2d),minval(p%ifield_2d)
              elseif(p%Ndim==3) then
                 write(*,*) 'MAX/MIN==>',maxval(p%ifield_3d),minval(p%ifield_3d)
              else
              endif
          else
          endif
        ENDIF

        p => p%next
      ENDDO   ! p 

   end subroutine da_check_grid_content_wrfjedi

   !***********************************************************************
   !
   !  subroutine da_make_subpool_wrfjedi
   !
   !> \brief   make a pool from wrf grid
   !> \author  Ming Hu
   !> \date    20 November 2018
   !> \details
   !>  Given grid A, create pool B as a subset of the fields in A
   !
   !-----------------------------------------------------------------------
   subroutine da_make_subpool_wrfjedi(domain_grid, pool_c, nsize, fieldname, nfields)

      implicit none

      type (domain), pointer, intent(in) :: domain_grid
      type (wrfjedi_pool_type), pointer, intent(out) :: pool_c
      character (len=*), intent(in) :: fieldname(:)
      integer, intent(out) :: nfields
      integer, intent(in) :: nsize
!      type (wrfjedi_pool_type), pointer :: pool_c

      type (wrfjedi_pool_iterator_type) :: poolItr

      type (field0DReal), pointer :: field0d
      type (field1DReal), pointer :: field1d
      type (field2DReal), pointer :: field2d 
      type (field3DReal), pointer :: field3d
      type (field4DReal), pointer :: field4d
      type (field0DInteger), pointer :: ifield0d
      type (field1DInteger), pointer :: ifield1d
      type (field2DInteger), pointer :: ifield2d
      type (field3DInteger), pointer :: ifield3d

      integer :: ii
      INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe
      CHARACTER*80  dname, memord

      TYPE( fieldlist ), POINTER :: p
!
!
      nfields = 0
      write(*,*)'--Create a sub Pool from list of variable: ',nsize
      write(*,*)'--Create a sub Pool from list of variable: ',fieldname
!      call wrfjedi_pool_create_pool(pool_b, nsize)
      call wrfjedi_pool_create_pool(pool_c, nsize)

! get dimension for this domain
      call get_ijk_from_grid (  domain_grid ,                  &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

      p => domain_grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) )

        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN
          dname = p%DataName
          IF (p%Ntl.GT.0)  dname=dname(1:len(TRIM(dname))-2)
!          write(*,'(I4,A5,I5,3A15,A5,A10,A20)') p%Ndim, p%Type, p%Ntl, &
!                      TRIM(p%VarName),trim(p%DataName),trim(dname),&
!                      p%ProcOrient,trim(p%MemoryOrder) !,trim(p%dimname2)

          do ii=1, nsize
             if ( trim(fieldname(ii)).eq.(trim(p % DataName)) ) then
                IF      ( p%Type .EQ. 'r' ) THEN
                   IF ( p%Ndim .EQ. 0 ) THEN
                      write(*,*) ' reading 0D real variable ', TRIM(p%VarName)
                      allocate(field0d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), field0d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), field0d)
                      write(*,*) '0D real value: ', field0d % array
                      deallocate(field0d)
                   ELSE IF ( p%Ndim .EQ. 1 ) THEN
                      write(*,*) ' reading 1D real variable ', TRIM(p%VarName)
                      allocate(field1d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), field1d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), field1d)
                      write(*,*) '1D real MIN/MAX value: ', minval(field1d % array),maxval(field1d % array)
                      deallocate(field1d)
                   ELSE IF ( p%Ndim .EQ. 2 ) THEN
                      write(*,*) ' reading 2D real variable ', TRIM(p%VarName)
                      allocate(field2d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), field2d)
                      !call field2d%printFieldHead()
                      write(*,*) '2D real MIN/MAX value: ', minval(field2d % array),maxval(field2d % array)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), field2d)
                      deallocate(field2d)
                   ELSE IF ( p%Ndim .EQ. 3 ) THEN
                      write(*,*) ' reading 3D real variable ', TRIM(p%VarName)
                      allocate(field3d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), field3d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), field3d)
                      !call field3d%printFieldHead()
                      write(*,*) '3D real MIN/MAX value: ', minval(field3d % array),maxval(field3d % array)
                      deallocate(field3d)
                   ELSE IF ( p%Ndim .EQ. 4 ) THEN
                      write(*,*) ' reading 4D real variable ', TRIM(p%VarName)
                      allocate(field4d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), field4d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), field4d)
                      deallocate(field4d)
                   ENDIF
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                   write(*,*) ' reading double variable ', TRIM(p%VarName)
                   write(*,*) ' We do not have function to read double !'
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                   IF ( p%Ndim .EQ. 0 ) THEN
                      write(*,*) ' reading 0D integer variable ', TRIM(p%VarName)
                      allocate(ifield0d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), ifield0d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), ifield0d)
                      deallocate(ifield0d)
                   ELSE IF ( p%Ndim .EQ. 1 ) THEN
                      write(*,*) ' reading 1D integer variable ', TRIM(p%VarName)
                      allocate(ifield1d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), ifield1d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), ifield1d)
                      deallocate(ifield1d)
                   ELSE IF ( p%Ndim .EQ. 2 ) THEN
                      write(*,*) ' reading 2D integer variable ', TRIM(p%VarName)
                      allocate(ifield2d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), ifield2d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), ifield2d)
                      write(*,*) '2D integer MIN/MAX value: ', minval(ifield2d % array),maxval(ifield2d % array)
                      deallocate(ifield2d)
                   ELSE IF ( p%Ndim .EQ. 3 ) THEN
                      write(*,*) ' reading 3D integer variable ', TRIM(p%VarName)
                      allocate(ifield3d)
                      call wrfjedi_pool_get_gridfield(p, trim(p % DataName), ifield3d)
                      call wrfjedi_pool_add_field(pool_c, trim(p % DataName), ifield3d)
                      deallocate(ifield3d)
                   END IF
                ELSE IF ( p%Type .EQ. 'l' ) THEN
                   write(*,*) ' reading logical variable ', TRIM(p%VarName)
                   write(*,*) ' We do not have function to read logical !'
                ENDIF
                nfields = nfields + 1
             endif   !  trim(fieldname(ii)).eq.(trim(p % DataName))
          enddo

        ENDIF   !  p%ProcOrient

        p => p%next
      ENDDO   ! p 


!      call pool_print_members(pool_c, 'pool_c')
!      call wrfjedi_pool_clone_pool(pool_c, pool_b)
!      call wrfjedi_pool_empty_pool(pool_c)
!      call pool_print_members(pool_b, 'pool_b')
!      call wrfjedi_pool_destroy_pool(pool_c)
!      call wrfjedi_pool_destroy_pool(pool_b,.true.)

      if ( nsize.ne.nfields ) then
        write(*,*)'Missing field in the pool da_make_subpool nsize different: ',nsize,nfields
      end if
!
   end subroutine da_make_subpool_wrfjedi
!
!   
   !***********************************************************************
   !
   !  function da_common_vars
   !
   !> \author  Gael Descombes
   !> \date    26 December 2017
   !> \details
   !>  Count the number of fields in a Pool related to a list of fields
   !
   !-----------------------------------------------------------------------
   function da_common_vars(pool_a, fieldname) result(nsize0)

      implicit none
      type (wrfjedi_pool_type), pointer :: pool_a
      type (wrfjedi_pool_iterator_type) :: poolItr
      character (len=*) :: fieldname(:)
      integer :: ii, jj, nsize, nsize0

      nsize0 = 0
      nsize  = size(fieldname)
      write(0,*)'da_common_vars size: ',nsize
      call wrfjedi_pool_begin_iteration(pool_a)

         do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )
            ! Pools may in general contain dimensions, namelist options, fields, or other pools,
            ! so we select only those members of the pool that are fields
            if (poolItr % memberType == WRFJEDI_POOL_FIELD) then
               ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
               if (poolItr % dataType == WRFJEDI_POOL_REAL) then
                  do ii=1, nsize
                     if ( trim(fieldname(ii)).eq.(trim(poolItr % memberName)) ) then
                        write(0,*)'Common field: '//trim(fieldname(ii))
                        nsize0 = nsize0 + 1
                     else if (( trim(fieldname(ii)).eq.'index_qv').and.(trim(poolItr % memberName).eq.'scalars')) then
                        write(0,*)'Common field: '//trim(fieldname(ii))
                        nsize0 = nsize0 + 1 
                     end if
                  end do
               end if
            end if
         end do

      write(0,*)'da_common_vars = ',nsize0

   end function da_common_vars



   !***********************************************************************
   !
   !  subroutine da_random
   !
   !> \brief   Performs random for pool A
   !> \author  Gael Descombes
   !> \date    January 2018
   !> \details
   !
   !-----------------------------------------------------------------------
   subroutine da_random(pool_a)

      implicit none

      type (wrfjedi_pool_type), intent(inout),pointer :: pool_a

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_a)

      do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  !call random_vector(r0d_ptr_a)
                  call random_number(r0d_ptr_a)
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  !call random_vector(r1d_ptr_a)
                  call random_number(r1d_ptr_a)
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  !call random_vector(r2d_ptr_a)
                  call random_number(r2d_ptr_a)
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  !call random_vector(r3d_ptr_a)
                  call random_number(r3d_ptr_a)
               end if

            end if
         end if
      end do

   end subroutine da_random

   !-----------------------------------------------------------------------
   !  subroutine da_operator
   !
   !> \brief   Performs A = A 'kind_op' B for pools A and B
   !> \author  Michael Duda
   !> \date    20 December 2017
   !> \details
   !>  Given two pools, A and B, where the fields in B are a subset of
   !>  the fields in A, this routine adds the fields in B to fields in A
   !>  with the same name. When A and B contain identical fields, this
   !>  is equivalent to A = A 'kind_op' B.
   !>  \modified by Gael DESCOMBES to apply diffferent operator
   !
   !-----------------------------------------------------------------------
   subroutine da_operator(kind_op, pool_a, pool_b, pool_c)

      implicit none

      type (wrfjedi_pool_type), pointer :: pool_a, pool_b
      type (wrfjedi_pool_type), pointer, optional :: pool_c
      character (len=*) :: kind_op

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a, r0d_ptr_b, r0d_ptr_c
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b, r1d_ptr_c
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b, r2d_ptr_c
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b, r3d_ptr_c

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_b)

      !write(0,*)'-------------------------------------------------'
      !write(0,*)' Operator ',trim(kind_op)
      !write(0,*)'-------------------------------------------------'

      do while ( wrfjedi_pool_get_next_member(pool_b, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r0d_ptr_b)
                  if (present(pool_c)) then
                     call wrfjedi_pool_get_array(pool_c, trim(poolItr % memberName), r0d_ptr_c)
                     r0d_ptr_a = 0.0_kind_real
                  end if
                  if ( trim(kind_op).eq.'add' ) then
                     r0d_ptr_a = r0d_ptr_a + r0d_ptr_b
                     if (present(pool_c)) then
                       r0d_ptr_a = r0d_ptr_b + r0d_ptr_c
                     else
                        r0d_ptr_a = r0d_ptr_a + r0d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'schur' ) then
                     if (present(pool_c)) then
                        r0d_ptr_a = r0d_ptr_b * r0d_ptr_c
                     else
                        r0d_ptr_a = r0d_ptr_a * r0d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'sub' ) then
                     if (present(pool_c)) then
                        r0d_ptr_a = r0d_ptr_b - r0d_ptr_c
                     else
                        r0d_ptr_a = r0d_ptr_a - r0d_ptr_b
                     end if
                  end if

               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r1d_ptr_b)
                  if (present(pool_c)) then
                     call wrfjedi_pool_get_array(pool_c, trim(poolItr % memberName), r1d_ptr_c)
                     r1d_ptr_a = 0.0_kind_real
                  end if
                  if ( trim(kind_op).eq.'add' ) then
                     if (present(pool_c)) then
                        r1d_ptr_a = r1d_ptr_b + r1d_ptr_c
                     else
                        r1d_ptr_a = r1d_ptr_a + r1d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'schur' ) then
                     if (present(pool_c)) then
                        r1d_ptr_a = r1d_ptr_b * r1d_ptr_c
                     else
                        r1d_ptr_a = r1d_ptr_a * r1d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'sub' ) then
                     if (present(pool_c)) then
                        r1d_ptr_a = r1d_ptr_b - r1d_ptr_c
                     else
                        r1d_ptr_a = r1d_ptr_a - r1d_ptr_b
                     end if
                  end if

               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r2d_ptr_b)
                  if (present(pool_c)) then
                     call wrfjedi_pool_get_array(pool_c, trim(poolItr % memberName), r2d_ptr_c)
                     r2d_ptr_a = 0.0_kind_real
                  end if
                  if ( trim(kind_op).eq.'add' ) then
                     write(0,*)'Operator_a add MIN/MAX: ',minval(r2d_ptr_a),maxval(r2d_ptr_a) 
                     write(0,*)'Operator_b add MIN/MAX: ',minval(r2d_ptr_b),maxval(r2d_ptr_b) 
                     if (present(pool_c)) then
                        r2d_ptr_a = r2d_ptr_b + r2d_ptr_c
                     else
                        write(*,*)'regular addition'
                        r2d_ptr_a = r2d_ptr_a + r2d_ptr_b
                     end if
                     write(0,*)'Operator2 add MIN/MAX: ',minval(r2d_ptr_a),maxval(r2d_ptr_a) 
                  else if ( trim(kind_op).eq.'schur' ) then
                     if (present(pool_c)) then
                        r2d_ptr_a = r2d_ptr_b * r2d_ptr_c
                     else
                        r2d_ptr_a = r2d_ptr_a * r2d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'sub' ) then
                     if (present(pool_c)) then
                        r2d_ptr_a = r2d_ptr_b - r2d_ptr_c
                     else
                        r2d_ptr_a = r2d_ptr_a - r2d_ptr_b
                     end if
                  end if

               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r3d_ptr_b)
                  if (present(pool_c)) then
                     call wrfjedi_pool_get_array(pool_c, trim(poolItr % memberName), r3d_ptr_c)
                     r3d_ptr_a = 0.0_kind_real
                  end if
                  if ( trim(kind_op).eq.'add' ) then
                     if (present(pool_c)) then
                        r3d_ptr_a = r3d_ptr_b + r3d_ptr_c
                     else
                        r3d_ptr_a = r3d_ptr_a + r3d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'schur' ) then
                     if (present(pool_c)) then
                        r3d_ptr_a = r3d_ptr_b * r3d_ptr_c
                     else
                        r3d_ptr_a = r3d_ptr_a * r3d_ptr_b
                     end if
                  else if ( trim(kind_op).eq.'sub' ) then
                     if (present(pool_c)) then
                        r3d_ptr_a = r3d_ptr_b - r3d_ptr_c
                     else
                        r3d_ptr_a = r3d_ptr_a - r3d_ptr_b
                     end if
                  end if
               end if

            end if
         end if
      end do

   end subroutine da_operator

   !***********************************************************************
   !
   !  subroutine da_self_mult
   !
   !> \brief   Performs A = A * zz for pool A, zz a real number
   !> \author  Gael Descombes
   !> \date    22 December 2017
   !> \details
   !
   !-----------------------------------------------------------------------
   subroutine da_self_mult(pool_a, zz)

      implicit none

      type (wrfjedi_pool_type), pointer :: pool_a
      real (kind=kind_real) :: zz

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_a)

      do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  r0d_ptr_a = r0d_ptr_a * zz
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  r1d_ptr_a = r1d_ptr_a * zz
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  r2d_ptr_a = r2d_ptr_a * zz
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  r3d_ptr_a = r3d_ptr_a * zz
               end if

            end if
         end if
      end do

   end subroutine da_self_mult

   
   !***********************************************************************
   !
   !  subroutine da_zeros
   !
   !> \brief   Performs A = 0. for pool A
   !> \author  Gael Descombes
   !> \date    22 December 2017
   !> \details
   !
   !-----------------------------------------------------------------------
   subroutine da_zeros(pool_a)

      implicit none

      type (wrfjedi_pool_type), pointer :: pool_a

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a
      real (kind=RKIND), dimension(:,:,:,:), pointer :: r4d_ptr_a

      integer, pointer :: i0d_ptr_a
      integer, dimension(:), pointer :: i1d_ptr_a
      integer, dimension(:,:), pointer :: i2d_ptr_a
      integer, dimension(:,:,:), pointer :: i3d_ptr_a

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_a)

      do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  r0d_ptr_a = 0.0_kind_real
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  r1d_ptr_a = 0.0_kind_real
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  r2d_ptr_a = 0.0_kind_real
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  r3d_ptr_a = 0.0_kind_real
               else if (poolItr % nDims == 4) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r4d_ptr_a)
                  r4d_ptr_a = 0.0_kind_real
               end if

            end if
            if (poolItr % dataType == WRFJEDI_POOL_INTEGER) then
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), i0d_ptr_a)
                  i0d_ptr_a = 0
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), i1d_ptr_a)
                  i1d_ptr_a = 0
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), i2d_ptr_a)
                  i2d_ptr_a = 0
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), i3d_ptr_a)
                  i3d_ptr_a = 0
               end if
            endif
         end if
      end do

   end subroutine da_zeros

   !***********************************************************************
   !
   !  subroutine da_setval
   !
   !> \brief   Performs A = Val_R. for pool A
   !> \author  Gael Descombes
   !> \date    22 December 2017
   !> \details
   !
   !-----------------------------------------------------------------------
   subroutine da_setval(pool_a,zz)

      implicit none

      type (wrfjedi_pool_type), pointer :: pool_a
      real (kind=kind_real) :: zz

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_a)

      do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  r0d_ptr_a = zz
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  r1d_ptr_a = zz
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  r2d_ptr_a = zz
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  r3d_ptr_a = zz
               end if

            end if
         end if
      end do

   end subroutine da_setval





   !***********************************************************************
   !
   !  subroutine da_axpy
   !
   !> \brief   Performs A = A + B * zz for pools A and B
   !> \author  Gael Descombes
   !> \date    20 December 2017
   !> \details
   !>  Given two pools, A and B, where the fields in B are a subset of
   !>  the fields in A, this routine adds the fields in B to fields in A
   !>  with the same name. When A and B contain identical fields, this
   !>  is equivalent to A = A + B.
   !
   !-----------------------------------------------------------------------
   subroutine da_axpy(pool_a, pool_b, zz)

      implicit none

      type (wrfjedi_pool_type), pointer :: pool_a, pool_b
      real (kind=kind_real) :: zz

      type (wrfjedi_pool_iterator_type) :: poolItr
      real (kind=RKIND), pointer :: r0d_ptr_a, r0d_ptr_b
      real (kind=RKIND), dimension(:), pointer :: r1d_ptr_a, r1d_ptr_b
      real (kind=RKIND), dimension(:,:), pointer :: r2d_ptr_a, r2d_ptr_b
      real (kind=RKIND), dimension(:,:,:), pointer :: r3d_ptr_a, r3d_ptr_b

      !
      ! Iterate over all fields in pool_b, adding them to fields of the same
      ! name in pool_a
      !
      call wrfjedi_pool_begin_iteration(pool_b)

      do while ( wrfjedi_pool_get_next_member(pool_b, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               if (poolItr % nDims == 0) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r0d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r0d_ptr_b)
                  r0d_ptr_a = r0d_ptr_a + r0d_ptr_b * zz
               else if (poolItr % nDims == 1) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r1d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r1d_ptr_b)
                  r1d_ptr_a = r1d_ptr_a + r1d_ptr_b * zz
               else if (poolItr % nDims == 2) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r2d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r2d_ptr_b)
                  r2d_ptr_a = r2d_ptr_a + r2d_ptr_b * zz
               else if (poolItr % nDims == 3) then
                  call wrfjedi_pool_get_array(pool_a, trim(poolItr % memberName), r3d_ptr_a)
                  call wrfjedi_pool_get_array(pool_b, trim(poolItr % memberName), r3d_ptr_b)
                  r3d_ptr_a = r3d_ptr_a + r3d_ptr_b * zz
               end if

            end if
         end if
      end do

   end subroutine da_axpy


   !***********************************************************************
   !
   !  subroutine da_gpnorm
   !
   !> \brief   Performs basic statistics min/max/norm given a pool
   !> \author  Ming Hu       
   !> \date    November 21 2018
   !> \details
   !>  Given a pool of fields, return min/max/norm array
   !
   !-----------------------------------------------------------------------
   
!   subroutine da_gpnorm(pool_a, dminfo, nf, pstat)
   subroutine da_gpnorm(pool_a, nf, pstat)

   implicit none
   type (wrfjedi_pool_type), intent(in),  pointer :: pool_a
!   type (dm_info), intent(in),  pointer :: dminfo
   integer,              intent(in) :: nf
   real(kind=kind_real), intent(inout)  :: pstat(3, nf)

   type (wrfjedi_pool_iterator_type) :: poolItr
   type (field0DReal), pointer :: field0d
   type (field1DReal), pointer :: field1d
   type (field2DReal), pointer :: field2d
   type (field3DReal), pointer :: field3d
   type (field2DInteger), pointer :: ifield2d
   real(kind=kind_real) :: globalSum, globalMin, globalMax, dimtot, dimtot_global, prodtot

   integer :: jj, ndims
   INTEGER :: sp1,ep1,sp2,ep2,sp3,ep3

   pstat = 0.0_kind_real

   !
   ! Iterate over all fields in pool_a
   ! name in pool_a
   !
   call wrfjedi_pool_begin_iteration(pool_a)
   jj = 1

      do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         ! Pools may in general contain dimensions, namelist options, fields, or other pools,
         ! so we select only those members of the pool that are fields
         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then

            ! Fields can be integer, logical, or real. Here, we operate only on real-valued fields
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               ! Depending on the dimensionality of the field, we need to set pointers of
               ! the correct type
               ndims = poolItr % nDims

!               write(*,*)'gpnorm variable: ',trim(poolItr % memberName), ndims

               if (ndims == 0) then
                  allocate(field0d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field0d)
                  pstat(1,jj) = field0d%array
                  pstat(2,jj) = field0d%array
                  pstat(3,jj) = field0d%array
                  deallocate(field0d)
                  
               elseif (ndims == 1) then
                  allocate(field1d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field1d)
                  call field1d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot = real((ep1-sp1+1),kind_real)
                  prodtot = sum(field1d % array(sp1:ep1)**2 )
!                  call wrfjedi_dmpar_sum_real(dminfo, dimtot, dimtot_global)
!                  call wrfjedi_dmpar_sum_real(dminfo, prodtot, globalSum)
!                  call wrfjedi_dmpar_min_real(dminfo, minval(field1d % array(1:solveDim1)), globalMin)
!                  call wrfjedi_dmpar_max_real(dminfo, maxval(field1d % array(1:solveDim1)), globalMax)
                  globalMin=minval(field1d % array(sp1:ep1))
                  globalMax=maxval(field1d % array(sp1:ep1))
                  pstat(1,jj) = globalMin
                  pstat(2,jj) = globalMax
                  globalSum=prodtot
                  dimtot_global=dimtot
                  pstat(3,jj) = sqrt( globalSum / dimtot_global )
                  deallocate(field1d)

               else if (ndims == 2) then
                  allocate(field2d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field2d)
                  call field2d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot  = real((ep1-sp1+1)*(ep2-sp2+1),kind_real)
                  prodtot = sum(field2d % array(sp1:ep1,sp2:ep2)**2 )
!                  call wrfjedi_dmpar_sum_real(dminfo, dimtot, dimtot_global)
!                  call wrfjedi_dmpar_sum_real(dminfo, prodtot, globalSum)
!                  call wrfjedi_dmpar_min_real(dminfo, minval(field2d % array(1:solveDim2,1:solveDim1)), globalMin)
!                  call wrfjedi_dmpar_max_real(dminfo, maxval(field2d % array(1:solveDim2,1:solveDim1)), globalMax)
                  globalMin=minval(field2d % array(sp1:ep1,sp2:ep2))
                  globalMax=maxval(field2d % array(sp1:ep1,sp2:ep2))
                  pstat(1,jj) = globalMin
                  pstat(2,jj) = globalMax
                  globalSum=prodtot
                  dimtot_global=dimtot
                  pstat(3,jj) = sqrt( globalSum / dimtot_global )
                  deallocate(field2d)

               else if (ndims == 3) then
                  allocate(field3d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field3d)
                  call field3d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot  = real((ep1-sp1+1)*(ep2-sp2+1)*(ep3-sp3+1),kind_real)
                  prodtot = sum(field3d % array(sp1:ep1,sp2:ep2,sp3:ep3)**2 )
!                  call wrfjedi_dmpar_sum_real(dminfo, dimtot, dimtot_global)
!                  call wrfjedi_dmpar_sum_real(dminfo, prodtot, globalSum)
!                  call wrfjedi_dmpar_min_real(dminfo, minval(field3d % array(1:solveDim3,1:solveDim2,1:solveDim1)), globalMin)
!                  call wrfjedi_dmpar_max_real(dminfo, maxval(field3d % array(1:solveDim3,1:solveDim2,1:solveDim1)), globalMax)
                  globalMin=minval(field3d % array(sp1:ep1,sp2:ep2,sp3:ep3))
                  globalMax=maxval(field3d % array(sp1:ep1,sp2:ep2,sp3:ep3))
                  pstat(1,jj) = globalMin
                  pstat(2,jj) = globalMax
                  globalSum=prodtot
                  dimtot_global=dimtot
                  pstat(1,jj) = globalMin
                  pstat(2,jj) = globalMax
                  pstat(3,jj) = sqrt( globalSum / dimtot_global )
                  deallocate(field3d)
               end if

!               write(*,*)'Variable: ',trim(poolItr % memberName),jj
!               write(*,*)'Min/Max stat: ',pstat(1,jj),pstat(2,jj),pstat(3,jj)
!               write(*,*)'' 

            elseif (poolItr % dataType == WRFJEDI_POOL_INTEGER) then

               ndims = poolItr % nDims
!               write(*,*)'gpnorm variable: ',trim(poolItr % memberName), ndims

               if (ndims == 2) then
                  allocate(ifield2d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), ifield2d)
                  call ifield2d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot  = real((ep1-sp1+1)*(ep2-sp2+1),kind_real)
                  prodtot = sum(ifield2d % array(sp1:ep1,sp2:ep2)**2 )
                  globalMin=minval(ifield2d % array(sp1:ep1,sp2:ep2))
                  globalMax=maxval(ifield2d % array(sp1:ep1,sp2:ep2))
                  pstat(1,jj) = globalMin
                  pstat(2,jj) = globalMax
                  globalSum=prodtot
                  dimtot_global=dimtot
                  pstat(3,jj) = sqrt( globalSum / dimtot_global )
                  deallocate(ifield2d)

               endif

!               write(*,*)'Variable: ',trim(poolItr % memberName),jj
!               write(*,*)'Min/Max stat: ',pstat(1,jj),pstat(2,jj),pstat(3,jj)
!               write(*,*)'' 

            end if
         end if
         jj = jj + 1

      end do

   end subroutine da_gpnorm


   !***********************************************************************
   !
   !  subroutine da_fldrms
   !
   !> \brief   Performs basic statistics min/max/norm given a pool
   !> \author  Gael Descombes
   !> \date    February 2018
   !> \details
   !>  Given a pool of fields, return min/max/norm array
   !
   !-----------------------------------------------------------------------

!   subroutine da_fldrms(pool_a, dminfo, fldrms)
   subroutine da_fldrms(pool_a, fldrms)

   implicit none
   type (wrfjedi_pool_type), intent(in),  pointer :: pool_a
!   type (dm_info), intent(in),  pointer :: dminfo
   real(kind=kind_real), intent(out) :: fldrms

   type (wrfjedi_pool_iterator_type) :: poolItr
   type (field1DReal), pointer :: field1d
   type (field2DReal), pointer :: field2d
   type (field3DReal), pointer :: field3d
   real(kind=kind_real) :: dimtot, dimtot_global, prodtot, prodtot_global 

   integer :: jj, ndims
   INTEGER :: sp1,ep1,sp2,ep2,sp3,ep3

   prodtot = 0.0_kind_real
   dimtot  = 0.0_kind_real

   !
   ! Iterate over all fields in pool_a
   ! named in pool_a
   !
   call wrfjedi_pool_begin_iteration(pool_a)
   jj = 1

   do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         if (poolItr % dataType == WRFJEDI_POOL_REAL) then
            if (poolItr % memberType == WRFJEDI_POOL_FIELD) then
            
               ndims = poolItr % nDims
               write(*,*)'fldrms variable: ',trim(poolItr % memberName),ndims

               if (ndims == 1) then
                  allocate(field1d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field1d)
                  call field1d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot = real((ep1-sp1+1),kind_real)
                  prodtot = sum(field1d % array(sp1:ep1)**2 )
                  deallocate(field1d)

               else if (ndims == 2) then
                  allocate(field2d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field2d)
                  call field2d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot  = real((ep1-sp1+1)*(ep2-sp2+1),kind_real)
                  prodtot = sum(field2d % array(sp1:ep1,sp2:ep2)**2 )
                  deallocate(field2d)

                  write(*,*)'fldrms dims: ',(ep1-sp1+1),(ep2-sp2+1)
                  write(*,*)'fldrms, dimtot, prodtot: ', dimtot, prodtot

                else if (ndims == 3) then
                  allocate(field3d)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field3d)
                  call field3d%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                  dimtot  = real((ep1-sp1+1)*(ep2-sp2+1)*(ep3-sp3+1),kind_real)
                  prodtot = sum(field3d % array(sp1:ep1,sp2:ep2,sp3:ep3)**2 )
                  deallocate(field3d)

               end if

               ! JJG: This output is useful for checks across multiple processors, really only need to do this comm once though (below)
!               call wrfjedi_dmpar_sum_real(dminfo, dimtot, dimtot_global)
!               call wrfjedi_dmpar_sum_real(dminfo, prodtot, prodtot_global)
!               write(*,*)'fldrms, dimtot_global, prodtot_global: ', dimtot_global, prodtot_global

            end if
         end if
         jj = jj + 1

      end do

!      call wrfjedi_dmpar_sum_real(dminfo, dimtot, dimtot_global)
!      call wrfjedi_dmpar_sum_real(dminfo, prodtot, prodtot_global)
      prodtot_global=prodtot
      dimtot_global=dimtot
      fldrms = sqrt(prodtot_global / dimtot_global)
      write(*,*)'fldrms = sqrt( prodtot_global / dimtot_global) : ', fldrms, prodtot_global, dimtot_global
      

  end subroutine da_fldrms


   !***********************************************************************
   !
   !  subroutine da_dot_product
   !
   !> \brief   Performs the dot_product given two pools of fields
   !> \author  Ming Hu
   !> \date    November 21 2018
   !> \details
   !>  Given two pools of fields, compute the dot_product
   !
   !-----------------------------------------------------------------------

!   subroutine da_dot_product(pool_a, pool_b, dminfo, zprod)
   subroutine da_dot_product(pool_a, pool_b, zprod)

   implicit none
   type (wrfjedi_pool_type), intent(in),  pointer :: pool_a, pool_b
!   type (dm_info), intent(in),  pointer :: dminfo
   real(kind=kind_real), intent(out) :: zprod

   type (wrfjedi_pool_iterator_type) :: poolItr
   type (field1DReal), pointer :: field1d_a, field1d_b
   type (field2DReal), pointer :: field2d_a, field2d_b
   type (field3DReal), pointer :: field3d_a, field3d_b
   real(kind=kind_real) :: fieldSum_local, zprod_local

   integer :: jj, ndims
   
   INTEGER :: sp1,ep1,sp2,ep2,sp3,ep3

   logical :: lSameDim
   !
   ! Iterate over all fields in pool_a
   ! named in pool_a
   !
   call wrfjedi_pool_begin_iteration(pool_a)

   zprod_local = 0.0_kind_real

   do while ( wrfjedi_pool_get_next_member(pool_a, poolItr) )

         if (poolItr % memberType == WRFJEDI_POOL_FIELD) then
            if (poolItr % dataType == WRFJEDI_POOL_REAL) then

               write(*,*)'variable: ',trim(poolItr % memberName)
               ndims = poolItr % nDims 

               if (ndims == 1) then
                  allocate(field1d_a,field1d_b)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field1d_a)
                  call wrfjedi_pool_get_field(pool_b, trim(poolItr % memberName), field1d_b)
                  lSameDim=.false.
                  call field1d_a%compareFieldHead(field1d_b%fieldhead,lSameDim)
                  if(lSameDim) then
                     call field1d_a%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                     fieldSum_local = sum(field1d_a % array(sp1:ep1) * field1d_b % array(sp1:ep1))
                  else
                     fieldSum_local=0.0
                     write(*,*) 'Warning: mismatch dimension for field1d_a and field1d_b !'
                  endif
                  zprod_local = zprod_local + fieldSum_local
                  write(*,*)'dotprod: ',trim(field1d_a % VarName), zprod_local,fieldSum_local
                  deallocate(field1d_a,field1d_b)

               else if (ndims == 2) then
                  allocate(field2d_a,field2d_b)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field2d_a)
                  call wrfjedi_pool_get_field(pool_b, trim(poolItr % memberName), field2d_b)
                  lSameDim=.false.
                  call field2d_a%compareFieldHead(field2d_b%fieldhead,lSameDim)
                  if(lSameDim) then
                     call field2d_a%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                     fieldSum_local = sum(field2d_a % array(sp1:ep1,sp2:ep2) * field2d_b % array(sp1:ep1,sp2:ep2))
                  else
                     fieldSum_local=0.0
                     write(*,*) 'Warning: mismatch dimension for field2d_a and field2d_b !'
                  endif
                  zprod_local = zprod_local + fieldSum_local
                  write(*,*)'dotprod: ',trim(field1d_a % VarName), zprod_local,fieldSum_local
                  deallocate(field2d_a,field2d_b)

               else if (ndims == 3) then
                  allocate(field3d_a,field3d_b)
                  call wrfjedi_pool_get_field(pool_a, trim(poolItr % memberName), field3d_a)
                  call wrfjedi_pool_get_field(pool_b, trim(poolItr % memberName), field3d_b)
                  lSameDim=.false.
                  call field3d_a%compareFieldHead(field3d_b%fieldhead,lSameDim)
                  if(lSameDim) then
                     call field3d_a%getFieldHeadDimP(sp1,ep1,sp2,ep2,sp3,ep3)
                     fieldSum_local = sum(field3d_a % array(sp1:ep1,sp2:ep2,sp3:ep3) &
                                       *  field3d_b % array(sp1:ep1,sp2:ep2,sp3:ep3))
                  else
                     fieldSum_local=0.0
                     write(*,*) 'Warning: mismatch dimension for field3d_a and field3d_b !'
                  endif
                  zprod_local = zprod_local + fieldSum_local
                  write(*,*)'dotprod: ',trim(field1d_a % VarName), zprod_local,fieldSum_local
                  deallocate(field3d_a,field3d_b)

               end if

            end if
         end if

      end do

!      call wrfjedi_dmpar_sum_real(dminfo, zprod_local, zprod)
      zprod=zprod_local
      write(*,*)'dotprod: Final result for patch = ',zprod

  end subroutine da_dot_product


!  subroutine cvt_oopswrfjedi_date(inString2,outString2,iconv)
!   
!     implicit none
!
!     character (len=*), intent(in) :: inString2     
!     character (len=*), intent(inout) :: outString2     
!     integer, intent(in) :: iconv
!     integer :: i, curLen
!     integer :: year, month, day, hour, minute, second
!
!     character (len=ShortStrKIND) :: timePart
!     character (len=ShortStrKIND) :: yearFormat
!     logical :: charExpand    
!     character (len=4) :: YYYY
!     character (len=2) :: MM, DD, h, m, s
!     character (len=21) :: outString, inString
! 
!     ! 2017-08-08T00:00:00Z OOPS/JSON format
!     ! 2010-10-24_02.00.00  WRFJEDI format
!     ! iconv=1: WRFJEDI --> OOPS/JSON
!     ! iconv=-1: OOPS/JSON --> WRFJEDI
!
!     if (iconv.eq.-1) then
!        YYYY = inString2(1:4)
!        MM   = inString2(6:7)     
!        DD   = inString2(9:10)     
!        h    = inString2(12:13)     
!        m    = inString2(15:16)     
!        s    = inString2(18:19)
!     else
!        YYYY = inString2(1:4)
!        MM   = inString2(6:7)
!        DD   = inString2(9:10)
!        h    = inString2(12:13)
!        m    = inString2(15:16)
!        s    = inString2(18:19)
!     end if
!
!     write(*,*)'cvt_oopswrfjedi_date instring: ',trim(YYYY),trim(MM),trim(DD),trim(h),trim(m),trim(s)
!     write(*,*)'cvt_oopswrfjedi_date input ',trim(instring2)
!
!     write(outString,*) ''
!     instring = trim(outstring2)     
!     
!     curLen = 0
!     charExpand = .false.
!     do i = 1, len_trim(inString)
!           if (inString(i:i) == '$' ) then
!               charExpand = .true.
!           else if (inString(i:i) /= '$') then
!               !write(*,*)'inString: ',trim(inString(i:i)),charExpand
!               if (charExpand) then
!                  select case (inString(i:i))
!                     case ('Y')
!                         outString = trim(outString) // trim(YYYY)
!                     case ('M')
!                         outString = trim(outString) // trim(MM)
!                     case ('D')
!                         outString = trim(outString) // trim(DD)
!                     case ('h')
!                         outString = trim(outString) // trim(h)
!                     case ('m')
!                         outString = trim(outString) // trim(m)
!                     case ('s')
!                         outString = trim(outString) // trim(s)
!                     case default
!                        write(*, *) 'ERROR: wrfjedi_expand_string option $', inString(i:i), ' is not a valid expansion character.'
!                        call wrfjedi_dmpar_global_abort('ERROR: wrfjedi_timekeeping')
!                  end select
!                  curLen = len_trim(outString)
!                  charExpand = .false.
!                  !write(*,*)'outString: ',trim(outString)
!               else
!                  outString(curLen+1:curLen+1) = outString2(i:i)
!                  curLen = curLen+1
!               end if
!           end if
!     end do
!
!     outString2 = trim(outString)
!     write(*,*)'cvt_oopswrfjedi_date output ',trim(outstring2)
!
!  end subroutine cvt_oopswrfjedi_date
!
!----------------------------------------------------------------------

subroutine r3_normalize(ax, ay, az)

   implicit none

   real(kind_real), intent(inout) :: ax, ay, az
   real(kind_real) :: mi

   mi = 1.0_kind_real / sqrt(ax**2 + ay**2 + az**2)

   ax = ax * mi
   ay = ay * mi
   az = az * mi

end subroutine r3_normalize

!-----------------------------------------------------------------------



!===============================================================================================================

end module wrfjedi4da_mod 
!
