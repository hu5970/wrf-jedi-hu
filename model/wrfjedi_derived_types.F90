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

   ! Derived type for storing fields
   type field2DReal

      ! Raw array holding field data on this block
      real (kind=RKIND), dimension(:,:), pointer :: array

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
      LOGICAL         :: boundary_array
      LOGICAL         :: restart

   end type field2DReal

   ! Derived type for storing fields
   type field2DInteger

      ! Raw array holding field data on this block
      integer, dimension(:,:), pointer :: array

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
      LOGICAL         :: boundary_array
      LOGICAL         :: restart
   end type field2DInteger

   contains

end module wrfjedi_derived_types
