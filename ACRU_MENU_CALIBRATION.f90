!###################################################################
! MODULE TITLE : M_SYSTEMLOG
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 27, 2015
! DATE REVISED : July 28, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed to format the LOG FILE.
! SUBROUTINE 1 : This subroutine will print out the start log
!                header for this script.
! SUBROUTINE 2 : This subroutine will print out the end log
!                header for this script.
!###################################################################
module m_systemlog

    implicit none
    character(11), parameter :: debugStat = '[ STATUS ] '
    character(11), parameter :: debugRes = '[ RESULT ] '
    character(67), parameter :: programHeader = "###################################################################"
    character(20), parameter :: dayStat = '             DATE : '
    character(20), parameter :: timeStat = '             TIME : '
    character(20), parameter :: etimeStat = '     ELAPSED TIME : '
    character(20), parameter :: logfileStat = '          LOGFILE : '
    character(20), parameter :: fileNameOpened =  '  FILENAME OPENED : '
    character(20), parameter :: fileStat =  '      FILE STATUS : '
    character(81), parameter :: lineHeader = '================================================================================='
    save

contains

    subroutine startprogramlog(unit_no)

        integer, intent(in) :: unit_no

        write(unit_no,*)
        write(unit_no,*) 'START OF PROGRAM. '
        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)
        write(unit_no,*) ' THE ACRU_MENU PROGRAM WILL COPY VALUES FROM A TAB-DELIMITED FILE. '
        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)

    end subroutine startprogramlog

    subroutine endprogramlog(unit_no)

        integer, intent(in) :: unit_no

        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)
        write(unit_no,*) '   THE ACRU_MENU PROGRAM HAS FINISHED UPDATING THE MENU FILE. '
        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)
        write(unit_no,*) 'END OF PROGRAM. '
     end subroutine endprogramlog

end module m_systemlog

!###################################################################
! MODULE TITLE : M_SYSTEMCHECK
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 28, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain DATE and TIME subroutines.
! SUBROUTINE 1 : This subroutine will calculate the date and time.
! SUBROUTINE 2 : This subroutine will calculate the elapsed time.
! SUBROUTINE 3 : This subroutine will check if the file was opened
!                successfully.
! SUBROUTINE 4 : This subroutine will check ISUBNO value.
!###################################################################
module m_systemcheck

    use m_systemlog, only: programHeader
    implicit none

contains

   subroutine datetimelog(date, datenow, timenow)

       character(len=8) :: dateinfo
       character(len=4) :: year, month*2, day*2
       character(len=2) :: hrs, min, sec*6
       character(len=10) :: timeinfo
       character(len=10), intent(out) :: date, datenow
       character(len=12), intent(out) :: timenow
       call date_and_time(dateinfo, timeinfo)
       year = dateinfo(1:4)
       month = dateinfo(5:6)
       day = dateinfo(7:8)
       date = year // '_' // month // '_' // day
       datenow = year // '-' // month // '-' // day
       hrs = timeinfo(1:2)
       min = timeinfo(3:4)
       sec = timeinfo(5:10)
       timenow = hrs // ':' // min // ':' // sec

   end subroutine datetimelog

   subroutine elapsedtime(elapsed_time, sys_count_0, sys_count_1, countrate)

       integer, intent(in) :: sys_count_0, sys_count_1, countrate
       real, intent(out) :: elapsed_time
       elapsed_time = 0
       elapsed_time = real(sys_count_1 - sys_count_0)/ real(countrate)

   end subroutine elapsedtime

   subroutine filestatcheck(status, unit_no)

       integer, intent(in) :: unit_no, status

       if (status==0) then
           write(unit_no,*) 'SUCCESSFULLY OPENED FILE.'
       end if
       if (status/=0) then
           write(unit_no,*) 'COULD NOT OPEN FILE.'
       end if

   end subroutine filestatcheck

    subroutine valuecheck(value, unit_no1, unit_no2)

        integer, intent(in) :: unit_no1, unit_no2, value

        if (value==0) then
            write(unit_no1,*)
            write(unit_no1,*) programHeader
            write(unit_no1,*)
            write(unit_no1,*) 'INVALID VALUE. TERMINATING PROGRAM'
            write(unit_no1,*)
            close(unit_no1)
            close(unit_no2)
            stop ' INVALID VALUE. TERMINATING PROGRAM. '
        end if

    end subroutine valuecheck

end module m_systemcheck

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

!###################################################################
! MAIN TITLE   : ACRU_MENU_CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Dr. Stefan W. Kienzle
! DATE EDITED  : May 19, 2008
! REVISED BY   : Charmaine Bonifacio
! DATE REVISED : July 28, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The program will copy values from a tab delimited
!                file that contains ALBEDO, CAY, ELAIM, ROOTA
!                COIAM and ICC
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! MODULES      : Must include m_systemcheck, m_systemlog and
!                m_calibration modules
! INPUT        : 1) MENU FILE = MENU
!                2) VARIABLE FILE = menu_variable.txt
! OUTPUT       : 1) Updated MENU File
!                2) LOG file
!###################################################################
program p_acru_menu_calibration

    use m_systemcheck
    use m_systemlog
    use m_calibration
    implicit none

    character(len=4), parameter :: menu = 'MENU'
    character(len=30), parameter :: menuvars = 'menu_variable.txt'
    character(len=30) :: outfile, infile, logrun, varfile
    character(len=80) :: dum, dum2, msg
    character(len=10) :: date, date_now, date_end
    character(len=12) :: time_now, time_end
    integer :: isubno
    integer :: count_0, count_1, count_rate, count_max
    integer :: line, line_num, i, l, p, ok, totalLine
    integer :: lineCoiam, lineCay, lineElaim, lineRoota, lineEof
    integer :: lineIcc, lineAlbedo
    logical :: ex
    real :: elapsed_time
    character(10), dimension(6) :: varType
    integer, dimension(6) :: varLineNum

!***********************************************************************
! SETUP START TIME
!***********************************************************************
    call system_clock(count_0, count_rate, count_max)
    call datetimelog(date, date_now, time_now)
!***********************************************************************
! START PROGRAM
!***********************************************************************
    logrun = 'LOGRUN_MENU_'//date//'.txt'
    inquire(file=logrun, exist=ex)
    write(*,*) debugStat, ' checking file: ', logrun
    if (ex) then
        open(unit=12,file=logrun,status='replace',iostat=ok)
    else
        open(unit=12,file=logrun,status='new',iostat=ok)
    endif
!***********************************************************************
! FORMATTING OPTIONS
!***********************************************************************
106 format(1X, A11, A20, F10.5)
107 format(1X, A11, A20, A30)
108 format(1X, A11, A20, A12)
109 format(1X, A11, A20, I4)
111 format(A80)
112 format(3X,I4)
113 format(1X, A11, A30, I7)
!  114 FORMAT(1X, A11, A15, I7)
115 format(1X, A11, A20, I7)
!116 format(1X, A11, A50, I7)
!117 FORMAT(10X, A11, A20, I7)
!118 format(10X, A11, ' CALIBRATED LINE ', I7, ' --- HRU # ', I4, ' OUT OF ', I4)
!119 format(10X,A11,A17,I7,A9,I4,A8,I4)
120 format(1X, A11, ' PROCESSED ', I7, ' NUMBER OF LINES.')
!***********************************************************************
! START LOG
!***********************************************************************
    call startprogramlog(12)
    write(12,108) debugStat, dayStat, date_now
    write(12,108) debugStat, timeStat, time_now
    write(12,*)
    write(12,107) debugStat, logfileStat, logrun
    write(12,109) debugStat, fileStat, ok
    write(12,*)
    write(12,*) '[ P R O C E S S I N G   M E N U   F I L E ] '
    write(12,*)
    write(12,*) debugStat, ' COPY MENU FILE AND RENAME TO MENU_OLD. '
    infile = menu
    outfile = menu//'_OLD'
    call system( "copy " // infile // " " // outfile)
    call system( "copy " // infile // " " // 'ORIGINAL_'//menu)
    write(12,*)
    write(12,*) '[ P R O C E S S I N G   R E Q U I R E D   F I L E S ] '
    write(12,*)
    varfile = menuvars
    open(unit=11,file=varfile,iostat=ok)
    write(12,*) '>> VARIABLE FILE.'
    call filestatcheck(ok,12)
    write(12,108) debugRes, fileNameOpened, varfile
    write(12,109) debugStat, fileStat, ok
    write(12,*)
    open(unit=20,file=outfile,iostat=ok)
    write(12,*) '>> OLD COPY OF MENU FILE.'
    call filestatcheck(ok,12)
    write(12,108) debugRes, fileNameOpened, outfile
    write(12,109) debugStat, fileStat, ok
    write(12,*)
    open(unit=30,file=infile,iostat=ok)
    write(12,*) '>> WORKING COPY OF MENU FILE.'
    call filestatcheck(ok,12)
    write(12,108) debugRes, fileNameOpened, infile
    write(12,109) debugStat, fileStat, ok
!***********************************************************************
! START PROCESSING MENU FILE - HOW MANY HRUS IN THIS MENU FILE?
!***********************************************************************
    isubno=0
    p=1
    do 898 while (p.lt.11)
        read(20,111) dum2
        p=p+1
898 end do
    read(20,112) isubno
    close(20)
    write(12,*)
    write(12,*) '[ O B T A I N   N U M B E R   O F   H R U  ] '
    write(12,*)
    write(12,113) debugRes, '       NUMBER OF HRU IN MENU : ', isubno
    call valuecheck(isubno,12,30)
!***********************************************************************
! EOF MENU FILE - HOW MANY LINES IN TOTAL?
!***********************************************************************
    open(unit=20,file=outfile)
    totalLine=1
    lineeof=23+((isubno+5)*145)+isubno
    do 899 while (totalLine.lt.lineeof)
        read(20,111) dum2
        totalLine=totalLine+1
899 end do
    close(20)
    write(12,*)
    write(12,*) '[ E O F   C H E C K ] '
    write(12,*)
    write(12,113) debugRes, '  COUNTED END OF FILE LINES : ', totalLine
    write(12,113) debugRes, '    CALCULATED LINES BY HRU : ', lineeof
!***********************************************************************
! CALCULATE LINE NUMBER FOR EACH VARIABLE!
! THEN OVERWRITE VALUES ONCE LINE IS FOUND. CONTINUE FOR X HRUS.
!***********************************************************************
    call calcvarline(lineAlbedo, isubno, 28)
    call calcvarline(lineCay, isubno, 53)
    call calcvarline(lineElaim, isubno, 54)
    call calcvarline(lineRoota, isubno, 56)
    call calcvarline(lineCoiam, isubno, 67)
    call calcvarline(lineIcc, isubno, 141)
    write(12,*)
    write(12,*) '[ S U M M A R Y   O F   L I N E S ] '
    write(12,*)
    write(12,115) debugStat,' ALBEDO : ',lineAlbedo
    write(12,115) debugStat,' CAY    : ',lineCay
    write(12,115) debugStat,' ELAIM  : ',lineElaim
    write(12,115) debugStat,' ROOTA  : ',lineRoota
    write(12,115) debugStat,' COIAM  : ',lineCoiam
    write(12,115) debugStat,' ICC  : ',lineIcc
    write(12,*)
! Initiate varType
    varType(1) = ' ALBEDO '
    varType(2) = ' CAY    '
    varType(3) = ' ELAIM  '
    varType(4) = ' ROOTA  '
    varType(5) = ' COIAM  '
    varType(6) = ' ICC    '
    varLineNum(1) = lineAlbedo
    varLineNum(2) = lineCay
    varLineNum(3) = lineElaim
    varLineNum(4) = lineRoota
    varLineNum(5) = lineCoiam
    varLineNum(6) = lineIcc
    do i=1, 6
      write(12,115) debugStat, varType(i)//': ', varLineNum(i)
    end do
!=================================================
! CHECK WHERE VARIABLES SHOULD BE OVERWRITTEN
    open(unit=20,file=outfile)
    line=1
    write(12,*)
    write(12,*) '[ C A L I B R A T I N G   M E N U   F I L E ] '
    write(12,*)
    do 900 while (line.lt.lineeof)
        l=1
        if(line.eq.lineAlbedo) then ! check where albedo should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, l, 1)
            line = line_num
            close(11)
        elseif(line.eq.lineCay) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, l, 2)
            line = line_num
            close(11)
        elseif(line.eq.lineElaim) then ! check where elaim should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, l, 3)
            line = line_num
            close(11)
        elseif(line.eq.lineRoota) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, l, 4)
            line = line_num
            close(11)
        elseif(line.eq.lineCoiam) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, l, 5)
            line = line_num
            close(11)
        elseif(line.eq.lineIcc) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, l, 6)
            line = line_num
            close(11)
        else ! simply read and copy lines
            read(20,111) dum
            write(30,111) dum
            line=line+1
        endif
900 end do
    close(20)
    write(12,*)
    write(*,120) debugStat, line
    write(12,120) debugStat, line
    write(12,*)
    msg = ' MENU CALIBRATED & CREATED BY CHARMAINE BONIFACIO. MENU SCRIPT VERSION --- '
    write(30,125) msg, date, ' | ', time_now
    125 format(A75,A10,A3,A5)
    endfile(30)
    close(30)
!***********************************************************************
! ELAPSED TIME
!***********************************************************************
    call system_clock(count_1, count_rate, count_max)
    call datetimelog(date, date_end, time_end)
!***********************************************************************
! END PROGRAM
!***********************************************************************
    write(12,108) debugStat, dayStat, date_end
    write(12,108) debugStat, timeStat, time_end
    call elapsedtime(elapsed_time, count_0, count_1, count_rate)
    write(12,106) debugStat, etimeStat, elapsed_time
    call endprogramlog(12)
    close(12)
end program p_acru_menu_calibration
