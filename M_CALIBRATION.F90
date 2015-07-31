!###################################################################
! MODULE TITLE : M_CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 30, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed for the program to work.
!###################################################################
module m_calibration

    use m_systemlog, only: debugStat, debugRes, lineHeader
    implicit none

    character(len=*), parameter:: format_line = '( A80 )'
    character(len=*), parameter:: format_var_header = '( 1X, A11, A50, I7 )'
    character(len=*), parameter:: format_icon_iswave = '( 66X,I1,5X,I1 )'
    character(len=*), parameter:: format_albedo = '( 1X,11(F4.2,1X),F4.2,6X,I1,5X,I1,3X,I4 )'
    character(len=*), parameter:: format_cerc = '( 1X,12(F4.2,1X),15X,I4 )'
    character(len=*), parameter:: format_icc = '( 2X,12(I3.2,2X),14X,I4 )'
    character(len=*), parameter:: format_calibrated = '( 10X, A11, A17, I7, A11, I4, A9, I4 )'

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  CALCVARLINE
!       DESCRIPTION  :  This subroutine will calculate the row line
!                       associated with the variable.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, OUTPUT the row line for each variable
!                       Integer, INPUT the total # of HRU
!                       Integer, INPUT the variable rank within Menu File.
!
!-------------------------------------------------------------------------------
    subroutine calcvarline(line_var, isubno, var_rank)

        integer, intent(out) :: line_var
        integer, intent(in) :: isubno, var_rank

        line_var = 0
        line_var = 23 + (isubno + 5) * var_rank

    end subroutine calcvarline

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  CALIBRATELINE
!       DESCRIPTION  :  This subroutine will calibrate the variables
!                       according to the line number.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, INPUT, unit number associated with file opened
!                       Integer, INPUT, unit number associated with file opened
!                       Integer, INPUT, unit number associated with file opened
!                       Integer, INPUT, unit number associated with file opened !                       Integer, INPUT
!                       Integer, INPUT, total number of HRU in MENU
!                       Integer, OUTPUT the total number of lines processed
!                       Integer, INPUT, the index associated with a variable
!
!-------------------------------------------------------------------------------
    subroutine calibrateline(unit_no, unit_oldMenu, unit_menu, unit_var, isubno, line, var_index)

        integer, intent(in) :: isubno, unit_no, unit_oldMenu, unit_menu, unit_var, var_index
        integer, intent(inout) :: line
        character(80) :: dum, dum2
        integer :: icons, iswave, i, l
        integer :: d1, d2
        integer, dimension(12) :: icc
        real, dimension(12) :: coiam, cay, elaim, roota, albedo

        l=1
        read(unit_var,*) dum2  ! header
        read(unit_var,*) dum2  ! header
        write(unit_no,*) lineHeader
        write(unit_no,format_var_header) debugStat, ' .... Menu Calibration starting from line >> ', line
        do 700 while (l.le.isubno)
            write(unit_no,101) debugStat,' PROCESSING LINE >> ', line
    101     format(1X, A11, A20, I7)
            read(unit_var,*)d1,d2, &
              (coiam(i),i=1,12),(cay(i),i=1,12), &
              (elaim(i),i=1,12),(roota(i),i=1,12), &
              (icc(i),i=1,12),(albedo(i),i=1,12)
            select case (var_index)
               case (1)
                   read(unit_oldMenu,format_icon_iswave)icons,iswave ! read original menu icons and iswave variables
                   write(unit_no,format_albedo)(albedo(i),i=1,12),icons,iswave,l
                   write(unit_menu,format_albedo)(albedo(i),i=1,12),icons,iswave,l
               case (2)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(cay(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(cay(i),i=1,12),(l)
               case (3)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(elaim(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(elaim(i),i=1,12),(l)
               case (4)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(roota(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(roota(i),i=1,12),(l)
               case (5)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(coiam(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(coiam(i),i=1,12),(l)
               case (6)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_icc)(icc(i),i=1,12),(l)
                   write(unit_menu,format_icc)(icc(i),i=1,12),(l)
           end select
           write(unit_no,format_calibrated) debugRes,' CALIBRATED LINE ', &
                                            line,' --- HRU # ',l,' OUT OF ',isubno
           l=l+1
           line=line+1
    700 end do

    end subroutine calibrateline

end module m_calibration
