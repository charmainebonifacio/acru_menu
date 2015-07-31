!###################################################################
! MODULE TITLE : M_SYSTEMCHECK
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 30, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will be used to check the date and time.
!###################################################################
module m_systemcheck

    implicit none

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  DATETIMELOG
!       DESCRIPTION  :  This subroutine will calculate the date and time.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Character, OUTPUT, date of the run (YYYY_MM_DD format)
!                       Character, OUTPUT, date of the run (YYYY-MM-DD format)
!                       Character, OUTPUT, time of the run
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
!       DESCRIPTION  :  This subroutine will calculate the elapsed time.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, OUTPUT, total time the program ran
!                       Integer, INPUT, start count
!                       Integer, INPUT, end count
!                       Integer, INPUT, rate count
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
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 27, 2015
! DATE REVISED : July 30, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed to format the LOG FILE.
!###################################################################
module m_systemlog

    implicit none
    character(11), parameter :: debugStat = '[ STATUS ] '
    character(11), parameter :: debugRes = '[ RESULT ] '
    character(11), parameter :: debugLog = '[ LOGGED ] '
    character(67), parameter :: programHeader = "###################################################################"
    character(20), parameter :: dayStat = '             DATE : '
    character(20), parameter :: timeStat = '             TIME : '
    character(20), parameter :: etimeStat = '     ELAPSED TIME : '
    character(20), parameter :: logfileStat = '          LOGFILE : '
    character(20), parameter :: fileNameOpened =  '  FILENAME OPENED : '
    character(20), parameter :: fileStat =  '      FILE STATUS : '
    character(81), parameter :: lineHeader = '================================================================================='
    character(len=*), parameter:: format_status_line = '( 1X, A11, A26 )'
    save

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  STARTPROGRAMLOG
!       DESCRIPTION  :  This subroutine will print out the start log
!                       header for this script.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, INPUT, unit number associated with file opened
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
!       DESCRIPTION  :  This subroutine will print out the end log
!                       header for this script.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, INPUT, unit number associated with file opened
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
!       DESCRIPTION  :  This subroutine will check if the file was opened
!                       successfully.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, INPUT, unit number associated with file opened
!                       Integer, INPUT, status value 0 is successful
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
!       DESCRIPTION  :  This subroutine will check the validity of the
!                       ISUBNO value before proceeding with the calibration.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, INPUT, value 0 means no ISUBNO found
!                    :  Integer, INPUT, unit number associated with file opened
!                    :  Integer, INPUT, unit number associated with file opened
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

!###################################################################
! MAIN TITLE   : P_ACRU_MENU_CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Dr. Stefan W. Kienzle
! DATE EDITED  : May 19, 2008
! REVISED BY   : Charmaine Bonifacio
! DATE REVISED : July 30, 2015
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
    integer :: lineCoiam, lineCay, lineElaim, lineRoota, lineEof
    integer :: lineIcc, lineAlbedo
    logical :: ex
    real :: elapsed_time
    character(10), dimension(6) :: varType
    integer, dimension(6) :: varLineNum

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
    call calcvarline(lineAlbedo, isubno, 28)
    call calcvarline(lineCay, isubno, 53)
    call calcvarline(lineElaim, isubno, 54)
    call calcvarline(lineRoota, isubno, 56)
    call calcvarline(lineCoiam, isubno, 67)
    call calcvarline(lineIcc, isubno, 141)
    write(12,*)
    write(12,*) '[ S U M M A R Y   O F   L I N E S ] '
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
      write(12,format_var_summary) debugStat, varType(i)//': ', varLineNum(i)
    end do
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
            call calibrateline(12, 20, 30, 11, isubno, line_num, 1)
            line = line_num
            close(11)
        elseif(line.eq.lineCay) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 2)
            line = line_num
            close(11)
        elseif(line.eq.lineElaim) then ! check where elaim should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 3)
            line = line_num
            close(11)
        elseif(line.eq.lineRoota) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 4)
            line = line_num
            close(11)
        elseif(line.eq.lineCoiam) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 5)
            line = line_num
            close(11)
        elseif(line.eq.lineIcc) then ! check where cay should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            call calibrateline(12, 20, 30, 11, isubno, line_num, 6)
            line = line_num
            close(11)
        else ! simply read and copy lines
            read(20,format_line) dum
            write(30,format_line) dum
            line=line+1
        endif
900 end do
    close(20)
    write(12,*) lineHeader
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
