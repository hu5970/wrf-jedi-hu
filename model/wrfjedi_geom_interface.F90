! (C) Copyright 2017 UCAR
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 

! ------------------------------------------------------------------------------
subroutine c_wrfjedi_geo_setup(c_key_self, c_conf) bind(c,name='wrfjedi_geo_setup_f90')
use iso_c_binding
use wrfjedi_geom_mod
implicit none
integer(c_int), intent(inout) :: c_key_self
type(c_ptr), intent(in) :: c_conf

type(wrfjedi_geom), pointer :: self

call wrfjedi_geom_registry%init()
call wrfjedi_geom_registry%add(c_key_self)
call wrfjedi_geom_registry%get(c_key_self, self)

call geo_setup(self, c_conf)

end subroutine c_wrfjedi_geo_setup

! ------------------------------------------------------------------------------

subroutine c_wrfjedi_geo_clone(c_key_self, c_key_other) bind(c,name='wrfjedi_geo_clone_f90')
use iso_c_binding
use wrfjedi_geom_mod
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: c_key_other

type(wrfjedi_geom), pointer :: self, other

call wrfjedi_geom_registry%add(c_key_other)
call wrfjedi_geom_registry%get(c_key_other, other)
call wrfjedi_geom_registry%get(c_key_self, self)

call geo_clone(self, other)

end subroutine c_wrfjedi_geo_clone

! ------------------------------------------------------------------------------

subroutine c_wrfjedi_geo_delete(c_key_self) bind(c,name='wrfjedi_geo_delete_f90')
use iso_c_binding
use wrfjedi_geom_mod
implicit none
integer(c_int), intent(inout) :: c_key_self     
type(wrfjedi_geom), pointer :: self

call wrfjedi_geom_registry%get(c_key_self, self)
call geo_delete(self)
call wrfjedi_geom_registry%remove(c_key_self)

end subroutine c_wrfjedi_geo_delete

! ------------------------------------------------------------------------------

subroutine c_wrfjedi_geo_info(c_key_self, c_nCellsGlobal, c_nCells, c_nCellsSolve, &
                                       c_nEdgesGlobal, c_nEdges, c_nEdgesSolve, &
                                       c_nVertLevels, c_nVertLevelsP1) &
                                       bind(c,name='wrfjedi_geo_info_f90')
use iso_c_binding
use wrfjedi_geom_mod
implicit none
integer(c_int), intent(in)    :: c_key_self
integer(c_int), intent(inout) :: &
   c_nCellsGlobal, c_nCells, c_nCellsSolve, &
   c_nEdgesGlobal, c_nEdges, c_nEdgesSolve, &
   c_nVertLevels, c_nVertLevelsP1

type(wrfjedi_geom), pointer :: self

call wrfjedi_geom_registry%get(c_key_self, self)
call geo_info(self, c_nCellsGlobal, c_nCells, c_nCellsSolve, &
                    c_nEdgesGlobal, c_nEdges, c_nEdgesSolve, &
                    c_nVertLevels, c_nVertLevelsP1)


end subroutine c_wrfjedi_geo_info
