!###################################################################
! MODULE TITLE : M_CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 28, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed for the program to work.
! SUBROUTINE 1 : This subroutine will calculate the row line
!                associated with the variable.
! SUBROUTINE 2 : This subroutine will calibrate the variables
!                according to the line number.
!###################################################################
module m_calibration

    use m_systemlog, only: debugStat, debugRes, lineHeader
    implicit none

contains

    subroutine calcvarline(line_var, isubno, var_row)

        integer, intent(out) :: line_var
        integer, intent(in) :: isubno, var_row

        line_var = 0
        line_var = 23 + (isubno + 5) * var_row

    end subroutine calcvarline

    subroutine calibrateline(unit_no, unit_oldMenu, unit_menu, unit_var, isubno, line, l, var_rank)

        integer, intent(in) :: isubno, unit_no, unit_oldMenu, unit_menu, unit_var, var_rank
        integer, intent(inout) :: line, l
        character(80) :: dum, dum2
        integer :: icons, iswave, i
        real :: d1, d2
        integer, dimension(12) :: icc
        real, dimension(12) :: coiam, cay, elaim, roota, albedo

        read(unit_var,*) dum2  ! header
        read(unit_var,*) dum2  ! header
        write(unit_no,*) lineHeader
        write(unit_no,100) debugStat, ' .... Menu Calibration starting from line >> ', line
    100 format(1X, A11, A50, I7)
        do 700 while (l.le.isubno)
            write(unit_no,101) debugStat,' PROCESSING LINE >> ', line
    101     format(1X, A11, A20, I7)
            read(unit_var,*)d1,d2, &
              (coiam(i),i=1,12),(cay(i),i=1,12), &
              (elaim(i),i=1,12),(roota(i),i=1,12), &
              (icc(i),i=1,12),(albedo(i),i=1,12)
            select case (var_rank)
               case (1)
                   read(unit_oldMenu,220)icons,iswave ! read original menu icons and iswave variables
                   write(unit_no,221)(albedo(i),i=1,12),icons,iswave,l
                   write(unit_menu,221)(albedo(i),i=1,12),icons,iswave,l
               case (2)
                   read(unit_oldMenu,102) dum
                   write(unit_no,222)(cay(i),i=1,12),(l)
                   write(unit_menu,222)(cay(i),i=1,12),(l)
               case (3)
                   read(unit_oldMenu,102) dum
                   write(unit_no,222)(elaim(i),i=1,12),(l)
                   write(unit_menu,222)(elaim(i),i=1,12),(l)
               case (4)
                   read(unit_oldMenu,102) dum
                   write(unit_no,222)(roota(i),i=1,12),(l)
                   write(unit_menu,222)(roota(i),i=1,12),(l)
               case (5)
                   read(unit_oldMenu,102) dum
                   write(unit_no,222)(coiam(i),i=1,12),(l)
                   write(unit_menu,222)(coiam(i),i=1,12),(l)
               case (6)
                   read(unit_oldMenu,102) dum
                   write(unit_no,223)(icc(i),i=1,12),(l)
                   write(unit_menu,223)(icc(i),i=1,12),(l)
           end select
    102    format(A80)
    220    format(66X,I1,5X,I1)
    221    format(1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4)
    222    format(1X,12(F4.2,1X),15X,I4)
    223    format(2X,12(I3.2,2X),14X,I4)
           write(unit_no,103) debugRes,line,l,isubno
    103    format(10X, A11, ' CALIBRATED LINE ', I7, ' --- HRU # ', I4, ' OUT OF ', I4)
           l=l+1
           line=line+1
    700 end do

    end subroutine calibrateline

end module m_calibration
