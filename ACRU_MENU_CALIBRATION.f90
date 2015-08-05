!###################################################################
! MODULE TITLE : M_SYSTEMCHECK
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : JULY 24, 2015
! DATE REVISED : JULY 30, 2015
! DESCRIPTION  : THE MODULE WILL BE USED TO CHECK THE DATE AND TIME.
!###################################################################
module m_systemcheck

    implicit none

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  DATETIMELOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE DATE AND TIME.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  CHARACTER, OUTPUT, DATE OF THE RUN (YYYY_MM_DD FORMAT)
!                       CHARACTER, OUTPUT, DATE OF THE RUN (YYYY-MM-DD FORMAT)
!                       CHARACTER, OUTPUT, TIME OF THE RUN
!
!-------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  ELAPSEDTIME
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE ELAPSED TIME.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, OUTPUT, TOTAL TIME THE PROGRAM RAN
!                       INTEGER, INPUT, START COUNT
!                       INTEGER, INPUT, END COUNT
!                       INTEGER, INPUT, RATE COUNT
!
!-------------------------------------------------------------------------------
   subroutine elapsedtime(elapsed_time, sys_count_0, sys_count_1, countrate)

       integer, intent(in) :: sys_count_0, sys_count_1, countrate
       real, intent(out) :: elapsed_time
       elapsed_time = 0
       elapsed_time = real(sys_count_1 - sys_count_0)/ real(countrate)

   end subroutine elapsedtime

end module m_systemcheck

!###################################################################
! MODULE TITLE : M_SYSTEMLOG
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : JULY 27, 2015
! DATE REVISED : AUGUST 1, 2015
! DESCRIPTION  : THE MODULE WILL CONTAIN VARIOUS SUBROUTINES
!                NEEDED TO FORMAT THE LOG FILE.
!###################################################################
module m_systemlog

    implicit none
    character(11), parameter :: debugStat = '[ STATUS ] '
    character(11), parameter :: debugRes = '[ RESULT ] '
    character(11), parameter :: debugLog = '[ LOGGED ] '
    character(36), parameter :: logHeader = '**************************************'
    character(67), parameter :: programHeader = "###################################################################"
    character(20), parameter :: dayStat = '             DATE : '
    character(20), parameter :: timeStat = '             TIME : '
    character(20), parameter :: etimeStat = '     ELAPSED TIME : '
    character(20), parameter :: logfileStat = '          LOGFILE : '
    character(20), parameter :: fileNameOpened =  '  FILENAME OPENED : '
    character(20), parameter :: fileStat =  '      FILE STATUS : '
    character(81), parameter :: sectionHeader = '================================================================================='
    character(len=*), parameter:: format_status_line = '( 1X, A11, A26 )'
    save

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  HEADERLOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT OUT THE HEADER CONTAINING
!                       SPECIFIC INFORMATION.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 1, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
    subroutine headerlog(unit_no)

        integer, intent(in) :: unit_no

        write(unit_no,*)
        write(unit_no,*) logHeader
        write(unit_no,*)
        write(unit_no,*) '        ACRU MENU LOG FILE'
        write(unit_no,*)
        write(unit_no,*) ' CREATED BY   : Charmaine Bonifacio'
        write(unit_no,*) ' DATE REVISED : August 1, 2015'
        write(unit_no,*)
        write(unit_no,*) logHeader

    end subroutine headerlog

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  STARTPROGRAMLOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT OUT THE START LOG
!                       HEADER FOR THIS SCRIPT.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  ENDPROGRAMLOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT OUT THE END LOG
!                       HEADER FOR THIS SCRIPT.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  FILESTATCHECK
!       DESCRIPTION  :  THIS SUBROUTINE WILL CHECK IF THE FILE WAS OPENED
!                       SUCCESSFULLY.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, STATUS VALUE 0 IS SUCCESSFUL
!
!-------------------------------------------------------------------------------
   subroutine filestatcheck(status, unit_no)

       integer, intent(in) :: unit_no, status

       if (status==0) then
           write(unit_no,format_status_line) debugStat, ' SUCCESSFULLY OPENED FILE.'
       end if
       if (status/=0) then
           write(unit_no,format_status_line) debugStat, ' COULD NOT OPEN FILE.'
       end if

   end subroutine filestatcheck

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  VALUECHECK
!       DESCRIPTION  :  THIS SUBROUTINE WILL CHECK THE VALIDITY OF THE
!                       ISUBNO VALUE BEFORE PROCEEDING WITH THE CALIBRATION.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, VALUE 0 MEANS NO ISUBNO FOUND
!                    :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                    :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
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

end module m_systemlog

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

!###################################################################
! MAIN TITLE   : P_ACRU_MENU_CALIBRATION
! CREATED BY   : CHARMAINE BONIFACIOO
! DATE CREATED : MAY 8, 2015
! DATE REVISED : AUGUST 1, 2015
! DESCRIPTION  : THE PROGRAM WILL COPY VALUES FROM A TAB DELIMITED
!                FILE THAT CONTAINS 22 VARIABLES:
!                SAUEF, DEPAHO, DEPBHO, WP1, WP2, FC1, FC2,
!                PO1, PO2, ABRESP, BFRESP, QFRESP, COFRU,
!                SMDDEP, COIAM, CAY, ELAIM, ROOTA,
!                ICC, ALBEDO, TMAXLR, TMINLR
! REQUIREMENT  : MUST RUN THE .EXE FILE WITHIN THE INPUT DIRECTORY.
! MODULES      : MUST INCLUDE M_SYSTEMCHECK, M_SYSTEMLOG AND
!                M_CALIBRATION MODULES
! INPUT        : 1) MENU FILE = MENU
!                2) VARIABLE FILE = MENU_VARIABLE.TXT
! OUTPUT       : 1) UPDATED MENU FILE
!                2) LOG FILE
!###################################################################
program p_acru_menu_calibration

    use m_systemcheck
    use m_systemlog
    use m_calibration
    implicit none

    character(len=4), parameter :: menu = 'MENU'
    character(len=*), parameter :: menuvars = 'menu_variable.txt'
    character(len=*), parameter:: format_header_line = '( A11, A80 )'
    character(len=*), parameter:: format_isubno = '( 3X,I4 )'
    character(len=*), parameter:: format_var_summary = '( 1X, A11, A20, I7 )'
    character(len=*), parameter:: format_line_summary = '( 1X, A11, A30, I7 )'
    character(len=*), parameter:: format_processed = '( 1X, A11, I7, A30 )'
    character(len=*), parameter:: format_etime = '(1X, A11, A20, F10.5)'
    character(len=*), parameter:: format_logfile = '(1X, A11, A20, A30)'
    character(len=*), parameter:: format_logstat = '(1X, A11, A20, A20)'
    character(len=*), parameter:: format_daytime = '(1X, A11, A20, A15)'
    character(len=*), parameter:: format_filestat = '(1X, A11, A20, I4)'
    character(len=*), parameter:: format_endmsg = '( A75,A10,A3,A5 )'
    character(len=30) :: outfile, infile, logrun, varfile
    character(len=80) :: dum, dum2, msg
    character(len=10) :: date, date_now, date_end
    character(len=12) :: time_now, time_end
    integer :: isubno
    integer :: count_0, count_1, count_rate, count_max
    integer :: line, line_num, i, p, ok, totalLine
    integer :: lineSauef, lineSoils, lineQfresp, lineTmxlr, lineTmnlr
    integer :: lineCoiam, lineCay, lineElaim, lineRoota, lineIcc, lineAlbedo
    integer :: lineEof
    logical :: ex
    real :: elapsed_time
    character(10), dimension(11) :: varType
    integer, dimension(11) :: varLineNum

!***********************************************************************
! START PROGRAM - DAY & TIME SETUP AND LOGFILE SETUP
    call system_clock(count_0, count_rate, count_max)
    call datetimelog(date, date_now, time_now)
    logrun = 'LOGRUN_MENU_'//date//'.txt'
    inquire(file=logrun, exist=ex)
    write(*,*) debugStat, ' checking file: ', logrun
    if (ex) then
        open(unit=12,file=logrun,status='replace',iostat=ok)
    else
        open(unit=12,file=logrun,status='new',iostat=ok)
    endif
    call headerlog(12)
    call startprogramlog(12)
    write(12,format_daytime) debugLog, dayStat, date_now
    write(12,format_daytime) debugLog, timeStat, time_now
    write(12,*)
    write(12,format_logfile) debugLog, logfileStat, logrun
    write(12,format_filestat) debugLog, fileStat, ok
    write(12,*)
    write(12,*) '[ C R E A T I N G   M E N U   F I L E ] '
    write(12,*)
    infile = menu
    outfile = menu//'_OLD'
    call system( "copy " // infile // " " // outfile)
    call system( "copy " // infile // " " // 'ORIGINAL_'//menu)
    write(12,*) debugStat, ' COPIED MENU FILE AND RENAMED TO MENU_OLD. '
    varfile = menuvars
    open(unit=11,file=varfile,iostat=ok)
    write(12,*) '>> PROCESSING VARIABLE FILE...'
    call filestatcheck(ok,12)
    write(12,format_logstat) debugStat, fileNameOpened, varfile
    write(12,format_filestat) debugStat, fileStat, ok
    write(12,*)
    open(unit=20,file=outfile,iostat=ok)
    write(12,*) '>> PROCESSING MENU_OLD COPY OF MENU FILE.'
    call filestatcheck(ok,12)
    write(12,format_logstat) debugStat, fileNameOpened, outfile
    write(12,format_filestat) debugStat, fileStat, ok
    write(12,*)
    open(unit=30,file=infile,iostat=ok)
    write(12,*) '>> PROCESSING WORKING COPY OF MENU FILE.'
    call filestatcheck(ok,12)
    write(12,format_logstat) debugStat, fileNameOpened, infile
    write(12,format_filestat) debugStat, fileStat, ok
!***********************************************************************
! START PROCESSING MENU FILE - HOW MANY HRUS IN THIS MENU FILE?
    isubno=0
    p=1
    do 898 while (p.lt.11)
        read(20,format_line) dum2
        p=p+1
898 end do
    read(20,format_isubno) isubno
    close(20)
    write(12,*)
    write(12,*) '[ O B T A I N   N U M B E R   O F   H R U  ] '
    write(12,*)
    write(12,format_var_summary) debugStat, ' # OF HRU IN MENU '//': ', isubno
    call valuecheck(isubno,12,30)
!***********************************************************************
! EOF MENU FILE - HOW MANY LINES IN TOTAL?
    open(unit=20,file=outfile)
    totalLine=1
    lineeof=23+((isubno+5)*145)+isubno
    do 899 while (totalLine.lt.lineeof)
        read(20,format_line) dum2
        totalLine=totalLine+1
899 end do
    close(20)
    write(12,*)
    write(12,*) '[ E O F   C H E C K ] '
    write(12,*)
    write(12,format_line_summary) debugRes, '  COUNTED END OF FILE LINES : ', totalLine
    write(12,format_line_summary) debugRes, '    CALCULATED LINES BY HRU : ', lineeof
!***********************************************************************
! CALCULATE LINE NUMBER FOR EACH VARIABLE!
! THEN OVERWRITE VALUES ONCE LINE IS FOUND. CONTINUE FOR X HRUS.
    call calcvarline(lineSauef, isubno, 9)
    call calcvarline(lineTmxlr, isubno, 20)
    call calcvarline(lineTmnlr, isubno, 21)
    call calcvarline(lineAlbedo, isubno, 28)
    call calcvarline(lineSoils, isubno, 43)
    call calcvarline(lineCay, isubno, 53)
    call calcvarline(lineElaim, isubno, 54)
    call calcvarline(lineRoota, isubno, 56)
    call calcvarline(lineQfresp, isubno, 66)
    call calcvarline(lineCoiam, isubno, 67)
    call calcvarline(lineIcc, isubno, 141)
    write(12,*)
    write(12,*) '[ S U M M A R Y   O F   L I N E S ] '
    write(12,*)
! Initiate varType
    varType(1) = ' SAUEF  '
    varType(2) = ' TMXLR  '
    varType(3) = ' TMNLR  '
    varType(4) = ' ALBEDO '
    varType(5) = ' SOILS  '
    varType(6) = ' CAY    '
    varType(7) = ' ELAIM  '
    varType(8) = ' ROOTA  '
    varType(9) = ' QFRESP '
    varType(10) = ' COIAM  '
    varType(11) = ' ICC    '
    varLineNum(1) = lineSauef
    varLineNum(2) = lineTmxlr
    varLineNum(3) = lineTmnlr
    varLineNum(4) = lineAlbedo
    varLineNum(5) = lineSoils
    varLineNum(6) = lineCay
    varLineNum(7) = lineElaim
    varLineNum(8) = lineRoota
    varLineNum(9) = lineQfresp
    varLineNum(10) = lineCoiam
    varLineNum(11) = lineIcc
    do i=1, 11
      write(12,format_var_summary) debugStat, varType(i)//': ', varLineNum(i)
    end do
    close(12)
    close(30)
    stop 'counting rows'
!***********************************************************************
! VARIABLE CALIBRATION STARTS HERE!
    write(12,*)
    write(12,*) '[ C A L I B R A T I N G   M E N U   F I L E ] '
    write(12,*)
    open(unit=20,file=outfile)
    line=1
    do 900 while (line.lt.lineeof)
        if(line.eq.lineAlbedo) then ! check where albedo should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 4)
            line = line_num
            close(11)
        elseif(line.eq.lineCay) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 6)
            line = line_num
            close(11)
        elseif(line.eq.lineElaim) then ! check where elaim should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 7)
            line = line_num
            close(11)
        elseif(line.eq.lineRoota) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 8)
            line = line_num
            close(11)
        elseif(line.eq.lineCoiam) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 10)
            line = line_num
            close(11)
        elseif(line.eq.lineIcc) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 11)
            line = line_num
            close(11)
        else ! simply read and copy lines
            read(20,format_line) dum
            write(30,format_line) dum
            line=line+1
        endif
900 end do
    close(20)
    write(12,*) sectionHeader
    write(12,*)
    write(*,format_processed) debugStat, line, ' = NUMBER OF PROCESSED LINES '
    write(12,format_processed) debugStat, line, ' = NUMBER OF PROCESSED LINES '
    write(12,*)
    msg = ' MENU CALIBRATED & CREATED BY CHARMAINE BONIFACIO. MENU SCRIPT VERSION --- '
    write(30,format_endmsg) msg, date, ' | ', time_now
    endfile(30)
    close(30)
!***********************************************************************
! END PROGRAM - ELAPSED TIME
    call system_clock(count_1, count_rate, count_max)
    call datetimelog(date, date_end, time_end)
    write(12,format_daytime) debugStat, dayStat, date_end
    write(12,format_daytime) debugStat, timeStat, time_end
    call elapsedtime(elapsed_time, count_0, count_1, count_rate)
    write(12,format_etime) debugStat, etimeStat, elapsed_time
    call endprogramlog(12)
    close(12)

end program p_acru_menu_calibration
