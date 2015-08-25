!###############################################################################
! MAIN TITLE   : P_ACRU_MENU_PARAMETERIZATION
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : MAY 8, 2015
! DATE REVISED : AUGUST 21, 2015
! DESCRIPTION  : THE PROGRAM WILL COPY VALUES FROM A TAB DELIMITED FILE THAT
!                CONTAINS 18 VARIABLES: TMAXLR, TMINLR, DEPAHO, DEPBHO, WP1, WP2,
!                FC1, FC2, PO1, PO2, ABRESP, BFRESP, QFRESP, COFRU, SMDDEP,
!                ISNOTP, IPSCOR, ISCREE
! REQUIREMENT  : MUST RUN THE .EXE FILE WITHIN THE INPUT DIRECTORY.
! MODULES      : MUST INCLUDE M_SYSTEMCHECK, M_SYSTEMLOG AND
!                M_ACRU_MENU MODULES
! INPUT        : 1) MENU FILE = MENU
!                2) VARIABLE FILE = MENU_PARAM.TXT
! OUTPUT       : 1) UPDATED MENU FILE
!                2) LOG FILE
!###############################################################################
program p_acru_menu_parameterization

    use m_systemcheck
    use m_systemlog
    use m_acru_menu
    implicit none

    character(len=4), parameter :: menu = 'MENU'
    character(len=*), parameter :: menuvars = 'MENU_PARAM.txt'
    character(len=*), parameter :: format_header_line = '( A11,A80 )'
    character(len=*), parameter :: format_error = '( 1X,A11,A40 )'
    character(len=*), parameter :: format_line_summary = '( 1X,A11,A30,I7 )'
    character(len=*), parameter :: format_processed = '( 1X,A11,I7,A56 )'
    character(len=*), parameter :: format_etime = '(1X, A11,A20,F10.5 )'
    character(len=*), parameter :: format_logfile = '( 1X,A11,A20,A30 )'
    character(len=*), parameter :: format_logstat = '( 1X,A11,A20,A20 )'
    character(len=*), parameter :: format_daytime = '( 1X,A11,A20,A15 )'
    character(len=*), parameter :: format_filestat = '( 1X,A11,A20,I4 )'
    character(len=*), parameter :: format_endmsg = '( A78, A10,A2,A5,A1 )'
    integer, parameter :: num_var = 5
    character(len=30) :: outfile, infile, logrun, varfile
    character(len=80) :: dum, msg
    character(len=10) :: date, date_now, date_end
    character(len=12) :: time_now, time_end
    integer :: isubno
    integer :: count_0, count_1, count_rate, count_max
    integer :: line, line_num, ok, totalLine
    integer :: lineTmxlr, lineTmnlr, lineSoils, lineStrmflw, lineSnow
    integer :: lineEof, valid_stat, blockIndex
    logical :: ex
    real :: elapsed_time
    character(len=50) :: blockString
    character(len=50), dimension(11) :: blockVariable
    integer, dimension(5) :: blockVarRow, blockContainer

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
    write(12,*) debugStat, ' COPIED MENU FILE AND RENAMED TO MENU_OLD. '
    call system( "copy " // infile // " " // 'ORIGINAL_'//menu)
    write(12,*) debugStat, ' COPIED MENU FILE AND RENAMED TO ORIGINAL_MENU. '
    varfile = menuvars
    call system( "copy " // varfile // " " // 'ORIGINAL_'//menuvars)
    write(12,*) debugStat, ' COPIED PARAM FILE AND RENAMED TO ORIGINAL_MENU_PARAM.TXT. '
    open(unit=11,file=varfile,iostat=ok)
    write(12,*)
    write(12,*) ' >> PROCESSING VARIABLE FILE...'
    write(12,*)
    call filestatcheck(ok,12)
    write(12,format_logstat) debugStat, fileNameOpened, varfile
    write(12,format_filestat) debugStat, fileStat, ok
    close(11)
    open(unit=20,file=outfile,iostat=ok)
    write(12,*)
    write(12,*)  '>> PROCESSING MENU_OLD COPY OF MENU FILE...'
    write(12,*)
    call filestatcheck(ok,12)
    write(12,format_logstat) debugStat, fileNameOpened, outfile
    write(12,format_filestat) debugStat, fileStat, ok
    close(20)
    open(unit=30,file=infile,iostat=ok)
    write(12,*)
    write(12,*) ' >> PROCESSING WORKING COPY OF MENU FILE...'
    write(12,*)
    call filestatcheck(ok,12)
    write(12,format_logstat) debugStat, fileNameOpened, infile
    write(12,format_filestat) debugStat, fileStat, ok
    close(30)
!***********************************************************************
! START PROCESSING MENU FILE
! START BY COUNTING HOW MANY HRUS ARE FOUND IN THIS MENU FILE.
    isubno=0
    open(unit=20,file=outfile)
    call findTotalCatchmentNumber(20, isubno)
    close(20)
    write(12,*)
    write(12,*) '[ O B T A I N   N U M B E R   O F   C A T C H M E N T S  ] '
    write(12,*)
    write(12,format_var_summary) debugStat, ' # OF HRU IN MENU '//': ', isubno
    open(unit=30,file=infile,iostat=ok)
    call valuecheck(isubno,12,30) ! if isubno = 0, program could not read value from file
!***********************************************************************
! THEN CALCULATE AND VALIDATE EOF FOR THE MENU FILE - HOW MANY LINES IN TOTAL?
    call calculateEOF(isubno, lineeof)
    open(unit=20,file=outfile)
    call calculateTOTLINES(20, totalLine)
    close(20)
    write(12,*)
    write(12,*) '[ E N D  O F  F I L E   C H E C K ] '
    write(12,*)
    write(12,format_line_summary) debugStat, '  COUNTED END OF FILE LINES : ', totalLine
    write(12,format_line_summary) debugStat, '    CALCULATED LINES BY HRU : ', lineeof
    call validateEOF(lineeof, totalLine, valid_stat)
!***********************************************************************
! EXIT IF EOF FOR THE MENU FILE IS INVALID
    if (valid_stat /= 0) then
      write(12,*)
      write(12,format_error) debugStat, ' CHECK IF THE MENU FILE IS CORRUPTED... '
      write(12,*)
      call system_clock(count_1, count_rate, count_max)
      call datetimelog(date, date_end, time_end)
      write(12,format_daytime) debugStat, dayStat, date_end
      write(12,format_daytime) debugStat, timeStat, time_end
      call elapsedtime(elapsed_time, count_0, count_1, count_rate)
      write(12,format_etime) debugStat, etimeStat, elapsed_time
      call endprogramlog(12)
      close(12)
      stop
    endif
!***********************************************************************
! CALCULATE LINE NUMBER FOR EACH VARIABLE!
! THEN OVERWRITE VALUES ONCE LINE IS FOUND. CONTINUE FOR X HRUS.
    call initiateVarCalibrationBlock(num_var, blockVariable)
    call initiateVarCalibrationContainer(num_var, blockContainer)
    call initiatiateVarLine(num_var, isubno, blockContainer, blockVarRow)
    write(12,*)
    write(12,*) '[ S U M M A R Y   O F   L I N E S ] '
    write(12,*)
    call printResults(12, num_var, blockVariable, blockContainer, blockVarRow)
!***********************************************************************
! VARIABLE CALIBRATION STARTS HERE!
    write(12,*)
    write(12,*) '[ M E N U   F I L E   P A R A M E T E R I Z A T I O N ] '
    write(12,*)
! Initiate specific lines of block.
    lineTmxlr   = blockVarRow(1)
    lineTmnlr   = blockVarRow(2)
    lineSoils   = blockVarRow(3)
    lineStrmflw = blockVarRow(4)
	  lineSnow    = blockVarRow(5)
    open(unit=20,file=outfile)
    open(unit=30,file=infile)
    line=1
    do 900 while (line < lineeof) ! Go thru MENU FILE once!
        if(line == lineTmxlr) then ! check where TMAXLR should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            blockIndex = 1
            blockString = blockVariable(blockIndex)
            call calibrateline(12, 20, 30, 11, isubno, line_num, blockIndex, blockString)
            line = line_num
            close(11)
        elseif(line == lineTmnlr) then ! check where TMINLR should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            blockIndex = 2
            blockString = blockVariable(blockIndex)
            call calibrateline(12, 20, 30, 11, isubno, line_num, blockIndex, blockString)
            line = line_num
            close(11)
        elseif(line == lineSoils) then ! check where SOILS should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            blockIndex = 3
            blockString = blockVariable(blockIndex)
            call calibrateline(12, 20, 30, 11, isubno, line_num, blockIndex, blockString)
            line = line_num
            close(11)
       elseif(line == lineStrmflw) then ! check where STREAMFLOW should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            blockIndex = 4
            blockString = blockVariable(blockIndex)
            call calibrateline(12, 20, 30, 11, isubno, line_num, blockIndex, blockString)
            line = line_num
            close(11)
       elseif(line == lineSnow) then ! check where SNOW should be overwritten
            open(unit=11,file=varfile)
            line_num = line
            blockIndex = 5
            blockString = blockVariable(blockIndex)
            call calibrateline(12, 20, 30, 11, isubno, line_num, blockIndex, blockString)
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
    write(12,format_processed) debugStat, line, ' = NUMBER OF PROCESSED LINES IN THE MENU PARAMETER FILE.  '
    write(12,*)
    msg = 'PARAMETERIZATION SCRIPT CREATED BY CHARMAINE BONIFACIO. VERSION AUGUST 2015. [ '
    write(30,format_endmsg) msg, date, '//', time_now,']'
    endfile(30)
    close(30)
    write(*,format_processed) debugStat, line, ' = NUMBER OF PROCESSED LINES IN THE MENU PARAMETER FILE. '
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

end program p_acru_menu_parameterization
