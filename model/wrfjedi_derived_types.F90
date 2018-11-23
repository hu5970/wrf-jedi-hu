! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://wrfjedi-dev.github.com/license.html
!
!***********************************************************************
!
!  wrfjedi_derived_types
!
!> \brief   WRFJEDI Kind definition module
!> \author  Ming Hu
!> \date    11/09/18
!> \details 
!> This module defines the kind types for basic fortran data types within WRFJEDI.
!
!-----------------------------------------------------------------------

module wrfjedi_derived_types

   use wrfjedi_kinds,      only : StrKIND, RKIND
   use module_domain_type, only : fieldlist
   private
   public :: field0DReal,field0DInteger
   public :: field1DReal,field1DInteger,field2DReal,field2DInteger
   public :: field3DReal,field3DInteger,field4DReal,field4DInteger

   type :: fieldhead
      ! Information used by the I/O layer
      CHARACTER*80    :: VarName
      CHARACTER*1     :: Type
      CHARACTER*1     :: ProcOrient  ! 'X' 'Y' or ' ' (X, Y, or non-transposed)
      CHARACTER*80    :: DataName
      CHARACTER*80    :: Description
      CHARACTER*80    :: Units
      CHARACTER*10    :: MemoryOrder
      CHARACTER*10    :: Stagger
      CHARACTER*80    :: dimname1
      CHARACTER*80    :: dimname2
      CHARACTER*80    :: dimname3
      LOGICAL         :: scalar_array
      INTEGER         :: Ndim

      INTEGER :: sd1,ed1,sd2,ed2,sd3,ed3
      INTEGER :: sm1,em1,sm2,em2,sm3,em3
      INTEGER :: sp1,ep1,sp2,ep2,sp3,ep3
      CHARACTER*80    :: MemberOf
      
      contains
         procedure :: sendFieldHead 
         procedure :: fillFieldHead 
         procedure :: printFieldHead 
         procedure :: compareFieldHead 
         procedure :: getFieldHeadDimD 
         procedure :: getFieldHeadDimM 
         procedure :: getFieldHeadDimP 
         procedure :: getFieldHeadDim 

   end type fieldhead

   ! Derived type for storing fields
   type,extends(fieldhead) :: field0DReal

      ! Raw array holding field data on this block
      real (kind=RKIND), pointer :: array=>NULL()

   end type field0DReal

   type,extends(fieldhead) :: field1DReal

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:), pointer :: array=>NULL()

   end type field1DReal

   type,extends(fieldhead) :: field2DReal

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:), pointer :: array=>NULL()

   end type field2DReal

   type,extends(fieldhead) :: field3DReal

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:,:), pointer :: array=>NULL()

   end type field3DReal

   type,extends(fieldhead) :: field4DReal

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:,:,:), pointer :: array=>NULL()

   end type field4DReal

   type,extends(fieldhead) :: field0DInteger

      ! Raw array holding field data on this block
      integer, pointer :: array=>NULL()

   end type field0DInteger

   type,extends(fieldhead) :: field1DInteger

      ! Raw array holding field data on this block
      integer, dimension(:), pointer :: array=>NULL()

   end type field1DInteger

   type,extends(fieldhead) :: field2DInteger

      ! Raw array holding field data on this block
      integer, dimension(:,:), pointer :: array=>NULL()

   end type field2DInteger

   type,extends(fieldhead) :: field3DInteger

      ! Raw array holding field data on this block
      integer, dimension(:,:,:), pointer :: array=>NULL()

   end type field3DInteger

   type,extends(fieldhead) :: field4DInteger

      ! Raw array holding field data on this block
      integer, dimension(:,:,:,:), pointer :: array=>NULL()

   end type field4DInteger

   contains

   subroutine sendFieldHead(this,infield)
!
      class(fieldhead) :: this
      type (fieldlist), intent(inout), pointer :: infield

      infield%VarName      = this%VarName
      infield%Type         = this%Type
      infield%ProcOrient   = this%ProcOrient
      infield%DataName     = this%DataName
      infield%Description  = this%Description
      infield%Units        = this%Units
      infield%MemoryOrder  = this%MemoryOrder
      infield%Stagger      = this%Stagger
      infield%dimname1     = this%dimname1
      infield%dimname2     = this%dimname2
      infield%dimname3     = this%dimname3
      infield%scalar_array = this%scalar_array
      infield%Ndim         = this%Ndim

      infield%sd1  =  this%sd1
      infield%ed1  =  this%ed1
      infield%sd2  =  this%sd2
      infield%ed2  =  this%ed2
      infield%sd3  =  this%sd3
      infield%ed3  =  this%ed3
   
      infield%sm1  =  this%sm1
      infield%em1  =  this%em1
      infield%sm2  =  this%sm2
      infield%em2  =  this%em2
      infield%sm3  =  this%sm3
      infield%em3  =  this%em3

      infield%sp1  =  this%sp1
      infield%ep1  =  this%ep1
      infield%sp2  =  this%sp2
      infield%ep2  =  this%ep2
      infield%sp3  =  this%sp3
      infield%ep3  =  this%ep3

   end subroutine sendFieldHead

   subroutine fillFieldHead(this,infield)
!
      class(fieldhead) :: this
      type (fieldlist), intent(in), pointer :: infield

      this%VarName      = infield%VarName
      this%Type         = infield%Type
      this%ProcOrient   = infield%ProcOrient
      this%DataName     = infield%DataName
      this%Description  = infield%Description
      this%Units        = infield%Units
      this%MemoryOrder  = infield%MemoryOrder
      this%Stagger      = infield%Stagger
      this%dimname1     = infield%dimname1
      this%dimname2     = infield%dimname2
      this%dimname3     = infield%dimname3
      this%scalar_array = infield%scalar_array
      this%Ndim         = infield%Ndim

      this%sd1  =  infield%sd1
      this%ed1  =  infield%ed1
      this%sd2  =  infield%sd2
      this%ed2  =  infield%ed2
      this%sd3  =  infield%sd3
      this%ed3  =  infield%ed3

      this%sm1  =  infield%sm1
      this%em1  =  infield%em1
      this%sm2  =  infield%sm2
      this%em2  =  infield%em2
      this%sm3  =  infield%sm3
      this%em3  =  infield%em3

      this%sp1  =  infield%sp1
      this%ep1  =  infield%ep1
      this%sp2  =  infield%sp2
      this%ep2  =  infield%ep2
      this%sp3  =  infield%sp3
      this%ep3  =  infield%ep3

   end subroutine fillFieldHead

   subroutine printFieldHead(this)
!
      class(fieldhead) :: this

      write(*,*) 'list file head:'
      write(*,*) 'VarName     = ',trim(this%VarName)
      write(*,*) 'Type        = ',this%Type
      write(*,*) 'ProcOrient  = ',this%ProcOrient
      write(*,*) 'DataName    = ',trim(this%DataName)
      write(*,*) 'Description = ',trim(this%Description)
      write(*,*) 'Units       = ',trim(this%Units)
      write(*,*) 'MemoryOrder = ',this%MemoryOrder
      write(*,*) 'Stagger     = ',this%Stagger
      write(*,*) 'scalar_array= ',this%scalar_array
      write(*,*) 'Ndim        = ',this%Ndim
      if(this%Ndim>0) then
         do i=1,this%Ndim
            if(i==1) write(*,*) 'dimname1    = ',trim(this%dimname1)
            if(i==2) write(*,*) 'dimname2    = ',trim(this%dimname2)
            if(i==3) write(*,*) 'dimname3    = ',trim(this%dimname3)
         enddo
      endif

      write(*,*) 'sd1 = ', this%sd1, 'ed1 = ', this%ed1
      write(*,*) 'sd2 = ', this%sd2, 'ed2 = ', this%ed2
      write(*,*) 'sd3 = ', this%sd3, 'ed3 = ', this%ed3
      write(*,*) 'sm1 = ', this%sm1, 'em1 = ', this%em1
      write(*,*) 'sm2 = ', this%sm2, 'em2 = ', this%em2
      write(*,*) 'sm3 = ', this%sm3, 'em3 = ', this%em3
      write(*,*) 'sp1 = ', this%sp1, 'ep1 = ', this%ep1
      write(*,*) 'sp2 = ', this%sp2, 'ep2 = ', this%ep2
      write(*,*) 'sp3 = ', this%sp3, 'ep3 = ', this%ep3

   end subroutine printFieldHead

   subroutine compareFieldHead(this,inhead,lSameDim)
!
      class(fieldhead) :: this
      type (fieldhead), intent(in) :: inhead
      logical, intent(out) :: lSameDim

      iCountDiff=0
      lSameDim=.false.

      if( trim(this%VarName)     /= trim(inhead%VarName) )     iCountDiff = iCountDiff + 1
      if(      this%Type         /=      inhead%Type     )     iCountDiff = iCountDiff + 1
      if( trim(this%MemoryOrder) /= trim(inhead%MemoryOrder) ) iCountDiff = iCountDiff + 1
      if(      this%Ndim         /=      inhead%Ndim     )     iCountDiff = iCountDiff + 1

      if( iCountDiff == 0 ) then
         if(this%Ndim >= 1) then
            if(this%sd1 /= inhead%sd1) iCountDiff = iCountDiff + 1
            if(this%ed1 /= inhead%ed1) iCountDiff = iCountDiff + 1
            if(this%sm1 /= inhead%sm1) iCountDiff = iCountDiff + 1
            if(this%em1 /= inhead%em1) iCountDiff = iCountDiff + 1
            if(this%sp1 /= inhead%sp1) iCountDiff = iCountDiff + 1
            if(this%ep1 /= inhead%ep1) iCountDiff = iCountDiff + 1
         endif
         if(this%Ndim >= 2) then
            if(this%sd2 /= inhead%sd2) iCountDiff = iCountDiff + 1
            if(this%ed2 /= inhead%ed2) iCountDiff = iCountDiff + 1
            if(this%sm2 /= inhead%sm2) iCountDiff = iCountDiff + 1
            if(this%em2 /= inhead%em2) iCountDiff = iCountDiff + 1
            if(this%sp2 /= inhead%sp2) iCountDiff = iCountDiff + 1
            if(this%ep2 /= inhead%ep2) iCountDiff = iCountDiff + 1
         endif

         if(this%Ndim >= 3) then
            if(this%sd3 /= inhead%sd3) iCountDiff = iCountDiff + 1
            if(this%ed3 /= inhead%ed3) iCountDiff = iCountDiff + 1
            if(this%sm3 /= inhead%sm3) iCountDiff = iCountDiff + 1
            if(this%em3 /= inhead%em3) iCountDiff = iCountDiff + 1
            if(this%sp3 /= inhead%sp3) iCountDiff = iCountDiff + 1
            if(this%ep3 /= inhead%ep3) iCountDiff = iCountDiff + 1
         endif
      endif
  
      if(iCountDiff==0) lSameDim=.true.

   end subroutine compareFieldHead

   subroutine getFieldHeadDimD(this,sd1,ed1,sd2,ed2,sd3,ed3)
      class(fieldhead) :: this

      INTEGER,intent(out) :: sd1,ed1,sd2,ed2,sd3,ed3

      sd1=this%sd1
      ed1=this%ed1
      sd2=this%sd2
      ed2=this%ed2
      sd3=this%sd3
      ed3=this%ed3

   end subroutine getFieldHeadDimD

   subroutine getFieldHeadDimM(this,sm1,em1,sm2,em2,sm3,em3)
      class(fieldhead) :: this

      INTEGER,intent(out) :: sm1,em1,sm2,em2,sm3,em3

      sm1=this%sm1
      em1=this%em1
      sm2=this%sm2
      em2=this%em2
      sm3=this%sm3
      em3=this%em3

   end subroutine getFieldHeadDimM

   subroutine getFieldHeadDimP(this,sp1,ep1,sp2,ep2,sp3,ep3)
      class(fieldhead) :: this

      INTEGER,intent(out) :: sp1,ep1,sp2,ep2,sp3,ep3

      sp1=this%sp1
      ep1=this%ep1
      sp2=this%sp2
      ep2=this%ep2
      sp3=this%sp3
      ep3=this%ep3

   end subroutine getFieldHeadDimP

   subroutine getFieldHeadDim(this,sd1,ed1,sd2,ed2,sd3,ed3,&
                                   sm1,em1,sm2,em2,sm3,em3,&
                                   sp1,ep1,sp2,ep2,sp3,ep3)
      class(fieldhead) :: this

      INTEGER,intent(out) :: sd1,ed1,sd2,ed2,sd3,ed3
      INTEGER,intent(out) :: sm1,em1,sm2,em2,sm3,em3
      INTEGER,intent(out) :: sp1,ep1,sp2,ep2,sp3,ep3

      call this%getFieldHeadDimD(sd1,ed1,sd2,ed2,sd3,ed3)

      call this%getFieldHeadDimM(sm1,em1,sm2,em2,sm3,em3)

      call this%getFieldHeadDimP( sp1,ep1,sp2,ep2,sp3,ep3)

   end subroutine getFieldHeadDim

end module wrfjedi_derived_types
