! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

module wrfjedi_kinds
  use, intrinsic :: iso_c_binding
  implicit none

  private
  public kind_real,I8KIND,RKIND,StrKIND,ShortStrKIND
  
  integer, parameter :: kind_real=c_double

  integer, parameter :: R4KIND = selected_real_kind(6)
  integer, parameter :: R8KIND = selected_real_kind(12)

  integer, parameter :: I8KIND = selected_int_kind(18)

  integer, parameter :: RKIND=R8KIND

  integer, parameter :: StrKIND = 512
  integer, parameter :: ShortStrKIND = 64


end module wrfjedi_kinds
