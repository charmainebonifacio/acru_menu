!###################################################################
! MODULE TITLE : M_CALIBRATION
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : JULY 24, 2015
! DATE REVISED : JULY 31, 2015
! DESCRIPTION  : THE MODULE WILL CONTAIN VARIOUS SUBROUTINES
!                NEEDED FOR THE PROGRAM TO WORK.
!###################################################################
module m_calibration

    use m_systemlog, only: debugStat, debugRes, sectionHeader
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
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE ROW LINE
!                       ASSOCIATED WITH THE VARIABLE.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, OUTPUT THE ROW LINE FOR EACH VARIABLE
!                       INTEGER, INPUT THE TOTAL # OF HRU
!                       INTEGER, INPUT THE VARIABLE RANK WITHIN MENU FILE.
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
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALIBRATE THE VARIABLES
!                       ACCORDING TO THE LINE NUMBER.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 31, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, TOTAL NUMBER OF HRU IN MENU
!                       INTEGER, OUTPUT THE TOTAL NUMBER OF LINES PROCESSED
!                       INTEGER, INPUT, THE INDEX ASSOCIATED WITH A VARIABLE
!
!-------------------------------------------------------------------------------
    subroutine calibrateline(unit_no, unit_oldMenu, unit_menu, unit_var, isubno, line, var_index)

        integer, intent(in) :: isubno, unit_no, unit_oldMenu, unit_menu, unit_var, var_index
        integer, intent(inout) :: line
        character(80) :: dum, dum2
        integer :: icons, iswave, ihemi, irun
        real :: sauef, depaho, depbho, wp1, wp2, fc1, fc2, po1, po2, abresp, bfresp
        real :: smddep, qfresp, cofru
        real :: clarea, elect, alat, wssize, adjump, disimp, stoimp
        integer ::  i, l, d1, d2
        integer, dimension(12) :: icc
        real, dimension(12) :: coiam, cay, elaim, roota, albedo, tmaxlr, tminlr

        l=1
        read(unit_var,*) dum2  ! header
        read(unit_var,*) dum2  ! header
        write(unit_no,*) sectionHeader
        write(unit_no,format_var_header) debugStat, ' .... Menu Calibration starting from line >> ', line
        do 700 while (l.le.isubno)
            write(unit_no,101) debugStat,' PROCESSING LINE >> ', line
    101     format(1X, A11, A20, I7)
            read(unit_var,*)d1, d2, sauef, depaho, depbho, &
              wp1, wp2, fc1, fc2, po1, po2, &
              abresp, bfresp, qfresp, cofru, smddep, &
              (coiam(i),i=1,12),(cay(i),i=1,12), &
              (elaim(i),i=1,12),(roota(i),i=1,12), &
              (icc(i),i=1,12),(albedo(i),i=1,12), &
              (tmaxlr(i),i=1,12),(tminlr(i),i=1,12)
            select case (var_index)
               case (4)
                   read(unit_oldMenu,format_icon_iswave)icons,iswave ! read original menu icons and iswave variables
                   write(unit_no,format_albedo)(albedo(i),i=1,12),icons,iswave,l
                   write(unit_menu,format_albedo)(albedo(i),i=1,12),icons,iswave,l
               case (6)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(cay(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(cay(i),i=1,12),(l)
               case (7)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(elaim(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(elaim(i),i=1,12),(l)
               case (8)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(roota(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(roota(i),i=1,12),(l)
               case (10)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc)(coiam(i),i=1,12),(l)
                   write(unit_menu,format_cerc)(coiam(i),i=1,12),(l)
               case (11)
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
