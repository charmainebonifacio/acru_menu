!###################################################################
! TITLE        : ACRU_MENU_CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Dr. Stefan W. Kienzle
! DATE EDITED  : May 19, 2008
! REVISED BY   : Charmaine Bonifacio
! DATE REVISED : July 21, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The program will copy values from a tab delimited
!                file that contains ALBEDO, COIAM, CAY, ELAIM, ROOTA
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! INPUT        : 1) MENU FILE = MENU
!                2) VARIABLE FILE = menu_variable.txt
! OUTPUT       : 1) Updated MENU File
!                2) LOG file
!###################################################################
PROGRAM ACRU_MENU_CALIBRATION
IMPLICIT NONE
CHARACTER(LEN=11), PARAMETER :: debugSTAT = '[ STATUS ] '
CHARACTER(LEN=11), PARAMETER :: debugRES = '[ RESULT ] '
INTEGER :: ISUBNO
INTEGER :: COUNT_0, COUNT_1, COUNT_RATE, COUNT_MAX
INTEGER :: LINE,I,L,P,OK,TOTALLINE
INTEGER :: LINECOIAM,LINECAY,LINEELAIM,LINEROOTA,LINEEOF
INTEGER :: LINEICC,LINEALBEDO,ICONS,ISWAVE
INTEGER, DIMENSION(12) :: ICC
REAL :: D1, D2
REAL, DIMENSION(12) :: COIAM, CAY, ELAIM, ROOTA, ALBEDO
CHARACTER(LEN=4), PARAMETER :: MENU = 'MENU'
CHARACTER(LEN=50), PARAMETER :: MENUVARS = 'menu_variable.txt'
CHARACTER(LEN=100) :: OUTFILE, INFILE, LOGRUN, VARFILE
CHARACTER(LEN=80) :: DUM, DUM2
CHARACTER(LEN=8) :: DATEINFO
CHARACTER(LEN=4) :: YEAR, MONTH*2, DAY*2
CHARACTER(LEN=2) :: HRS, MIN, SEC*6
CHARACTER(LEN=10) :: DATE, TIMEINFO, TIMENOW*12, DATENOW, TIMEEND*12, DATEEND
LOGICAL :: EX
!***********************************************************************
! Setup new MENU file
!***********************************************************************
      CALL DATE_AND_TIME(DATEINFO, TIMEINFO)
      CALL SYSTEM_CLOCK(COUNT_0, COUNT_RATE, COUNT_MAX)
      YEAR = DATEINFO(1:4)
      MONTH = DATEINFO(5:6)
      DAY = DATEINFO(7:8)
      DATE = YEAR // '_' // MONTH // '_' // DAY
      DATENOW = YEAR // '-' // MONTH // '-' // DAY
      HRS = TIMEINFO(1:2)
      MIN = TIMEINFO(3:4)
      SEC = TIMEINFO(5:10)
      TIMENOW = HRS // ':' // MIN // ':' // SEC
!***********************************************************************
! START PROGRAM
!***********************************************************************
      WRITE(*,*)
      WRITE(*,*) "###################################################################"
      WRITE(*,*) ' '
      WRITE(*,*) ' The ACRU_MENU program will COPY values from a tab-delimited file. '
      WRITE(*,*) ' '
      WRITE(*,*) "###################################################################"
      WRITE(*,*)
!***********************************************************************
! DATE
!***********************************************************************
    WRITE(*,*) debugSTAT, ' Start Date of log  -> ', DATENOW
      WRITE(*,*) debugSTAT, ' Start Time of log  -> ', TIMENOW
      WRITE(*,*)
      LOGRUN = 'LOGRUN_MENU_'//DATE//'.txt'
      INQUIRE(FILE=LOGRUN, EXIST=EX)
      WRITE(*,*) debugSTAT, ' Checking file: ', LOGRUN
      IF (EX) THEN
        OPEN(UNIT=12,FILE=LOGRUN,STATUS='REPLACE',IOSTAT=OK)
      ELSE
        OPEN(UNIT=12,FILE=LOGRUN,STATUS='NEW',IOSTAT=OK)
      ENDIF
      IF (OK/=0) THEN
        WRITE(*,*) debugRES, 'COULD NOT OPEN FILE.'
        STOP
      ENDIF
    WRITE(*,*) debugRES, ' File opened: ', LOGRUN
    WRITE(*,*) debugRES, ' File status ok = ', OK
      WRITE(12,*)
!***********************************************************************
! START LOG
!***********************************************************************
      WRITE(12,*) 'START OF PROGRAM. '
      WRITE(12,*)
      WRITE(12,*) "###################################################################"
      WRITE(12,*) ' '
      WRITE(12,*) ' The ACRU_MENU program will COPY values from a tab-delimited file. '
      WRITE(12,*) ' '
      WRITE(12,*) "###################################################################"
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' DATE -> ', DATENOW
      WRITE(12,*) debugSTAT, ' TIME -> ', TIMENOW
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' LOGFILE -> ', LOGRUN
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' Copy MENU file and rename to MENU_OLD. '
      INFILE = MENU
      OUTFILE = MENU//'_OLD'
      CALL SYSTEM( "copy " // INFILE // " " // OUTFILE)
      CALL SYSTEM( "copy " // INFILE // " " // MENU//'2')
      OPEN(UNIT=30,FILE=INFILE,IOSTAT=OK)
      IF (OK.EQ.0) THEN
        CLOSE(30, STATUS='DELETE')
      ENDIF
      WRITE(12,*) debugRES, ' Deleted old MENU file.'
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' Process files..... '
      VARFILE = MENUVARS
      OPEN(UNIT=11,FILE=VARFILE,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', VARFILE
      WRITE(*,*) debugSTAT, ' File status ok = ', OK
      WRITE(12,*) debugRES, ' VARIABLE FILE -> ', VARFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      OPEN(UNIT=20,FILE=OUTFILE,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', OUTFILE
      WRITE(*,*) debugSTAT, ' File status ok = ', OK
      WRITE(12,*) debugRES, ' MENUFILE COPY -> ', OUTFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      OPEN(UNIT=30,FILE=INFILE,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', INFILE
      WRITE(*,*) debugSTAT, ' File status ok = ', OK
      WRITE(12,*) debugRES, ' MENUFILE -> ', INFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
!***********************************************************************
! START PROCESSING MENU FILE - How many HRUs in this menu file?
!***********************************************************************
      ISUBNO=0
      P=1
      DO 898 WHILE (P.LT.11)
        READ(20,111) DUM2
        P=P+1
  898 CONTINUE
  111 FORMAT(A80)
      READ(20,112) ISUBNO
      WRITE(12,112) ISUBNO
  112 FORMAT(3X,I4)
      CLOSE(20)
      WRITE(*,*) debugRES, 'Number of HRUs = ',ISUBNO
      WRITE(12,*) debugRES, 'Number of HRUs = ',ISUBNO
      WRITE(12,*)
!***********************************************************************
! EOF MENU FILE - How many lines in total?
!***********************************************************************
      OPEN(UNIT=20,FILE=OUTFILE)
      TOTALLINE=1
      LINEEOF=23+((ISUBNO+5)*145)+ISUBNO
      DO 899 WHILE (TOTALLINE.LT.LINEEOF)
        READ(20,111) DUM2
        TOTALLINE=TOTALLINE+1
  899 END DO
      CLOSE(20)
      WRITE(*,*) 'Number of lines read? ',TOTALLINE
      WRITE(*,*) 'Number of lines as per HRU calculation? ',LINEEOF
      WRITE(*,*)
      WRITE(12,*) 'Number of lines read? ',TOTALLINE
      WRITE(12,*) 'Number of lines as per HRU calculation? ',LINEEOF
      WRITE(12,*)
!***********************************************************************
! Calculate line number for each variable!
! Then overwrite values once line is found. Continue for X HRUs.
!***********************************************************************
      LINEALBEDO=23+((ISUBNO+5)*28)
      LINECAY   =23+((ISUBNO+5)*53)
      LINEELAIM =23+((ISUBNO+5)*54)
      LINEROOTA =23+((ISUBNO+5)*56)
      LINECOIAM =23+((ISUBNO+5)*67)
      LINEICC   =23+((ISUBNO+5)*141)
      WRITE(*,*) 'ALBEDO Line = ',LINEALBEDO
      WRITE(*,*) '   CAY Line = ',LINECAY
      WRITE(*,*) ' ELAIM Line = ',LINEELAIM
      WRITE(*,*) ' ROOTA Line = ',LINEROOTA
      WRITE(*,*) ' COIAM Line = ',LINECOIAM
      WRITE(*,*) '   ICC Line = ',LINEICC
      WRITE(*,*)
      WRITE(12,*) 'ALBEDO Line = ',LINEALBEDO
      WRITE(12,*) '   CAY Line = ',LINECAY
      WRITE(12,*) ' ELAIM Line = ',LINEELAIM
      WRITE(12,*) ' ROOTA Line = ',LINEROOTA
      WRITE(12,*) ' COIAM Line = ',LINECOIAM
      WRITE(12,*) '   ICC Line = ',LINEICC
      WRITE(12,*)
      OPEN(UNIT=20,FILE=OUTFILE)
      LINE=1
      DO 900 WHILE (LINE.LT.LINEEOF)
        L=1
!=================================================
! CHECK WHERE VARIABLES SHOULD BE OVERWRITTEN
        IF(LINE.EQ.LINEALBEDO) THEN ! CHECK WHERE ALBEDO SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,*) debugSTAT, ' .... ALBEDO CALIBRATION STARTING FROM LINE >> ', LINE
          DO 901 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,*) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,220)ICONS,ISWAVE ! Read original MENU ICONS and ISWAVE variables
  220       FORMAT(66X,I1,5X,I1)
            WRITE(12,221)(ALBEDO(I),I=1,12),ICONS,ISWAVE,L
            WRITE(30,221)(ALBEDO(I),I=1,12),ICONS,ISWAVE,L
  221       FORMAT(1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4)
            WRITE(*,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            WRITE(12,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            L=L+1
          LINE=LINE+1
  901     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
        ELSEIF(LINE.EQ.LINECAY) THEN ! CHECK WHERE CAY SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,*) debugSTAT, ' .... CAY CALIBRATION STARTING FROM LINE >> ', LINE
          DO 902 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,*) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(CAY(I),I=1,12),(L)
            WRITE(30,222)(CAY(I),I=1,12),(L)
  222       FORMAT(1X,12(F4.2,','),15X,I4)
            WRITE(*,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            WRITE(12,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            L=L+1
          LINE=LINE+1
  902     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
        ELSEIF(LINE.EQ.LINEELAIM) THEN ! CHECK WHERE ELAIM SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,*) debugSTAT, ' .... ELAIM CALIBRATION STARTING FROM LINE >> ', LINE
          DO 903 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,*) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(ELAIM(I),I=1,12),(L)
            WRITE(30,222)(ELAIM(I),I=1,12),(L)
            WRITE(*,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            WRITE(12,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            L=L+1
          LINE=LINE+1
  903     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
        ELSEIF(LINE.EQ.LINEROOTA) THEN ! CHECK WHERE ROOTA SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,*) debugSTAT, ' .... ROOTA CALIBRATION STARTING FROM LINE >> ', LINE
          DO 904 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,*) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(ROOTA(I),I=1,12),(L)
            WRITE(30,222)(ROOTA(I),I=1,12),(L)
            WRITE(*,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            WRITE(12,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            L=L+1
          LINE=LINE+1
  904     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
        ELSEIF(LINE.EQ.LINECOIAM) THEN ! CHECK WHERE COIAM SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,*) debugSTAT, ' .... COIAM CALIBRATION STARTING FROM LINE >> ', LINE
          DO 905 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,*) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(COIAM(I),I=1,12),(L)
            WRITE(30,222)(COIAM(I),I=1,12),(L)
            WRITE(*,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            WRITE(12,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            L=L+1
          LINE=LINE+1
  905     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
        ELSEIF(LINE.EQ.LINEICC) THEN ! CHECK WHERE ICC SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,*) debugSTAT, ' .... ICC CALIBRATION STARTING FROM LINE >> ', LINE
          DO 906 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,*) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,223)(ICC(I),I=1,12),(L)
            WRITE(30,223)(ICC(I),I=1,12),(L)
  223       FORMAT(2X,12(I3.2,2X),14X,I4)
            WRITE(*,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            WRITE(12,*) debugRES, ' CALIBRATED LINE ',LINE, '--- HRU # ',L, ' OUT OF ',ISUBNO
            L=L+1
          LINE=LINE+1
  906     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
       ELSE ! SIMPLY READ AND COPY LINES
          READ(20,111) DUM
          WRITE(30,111) DUM
          WRITE(*,*) debugSTAT, ' PROCESSED LINE ',LINE
          WRITE(12,*) debugSTAT, ' PROCESSED LINE ',LINE
          LINE=LINE+1
        ENDIF
  900 END DO
      CLOSE(20)
      DUM2 = ' MENU SCRIPT VERSION JULY 2015 --- MENU CALIBRATED & CREATED BY CHARMAINE BONIFACIO '
      WRITE(30,111) DUM2
      ENDFILE(30)
      CLOSE(30)
!***********************************************************************
! ELAPSED TIME
!***********************************************************************
      CALL DATE_AND_TIME(DATEINFO, TIMEINFO)
      CALL SYSTEM_CLOCK(COUNT_1, COUNT_RATE, COUNT_MAX)
      YEAR = DATEINFO(1:4)
      MONTH = DATEINFO(5:6)
      DAY = DATEINFO(7:8)
      DATE = YEAR // '_' // MONTH // '_' // DAY
      DATEEND = YEAR // '-' // MONTH // '-' // DAY
      HRS = TIMEINFO(1:2)
      MIN = TIMEINFO(3:4)
      SEC = TIMEINFO(5:10)
      TIMEEND = HRS // ':' // MIN // ':' // SEC
!***********************************************************************
! END PROGRAM
!***********************************************************************
      WRITE(*,*) "###################################################################"
      WRITE(*,*) ' '
      WRITE(*,*) '   The ACRU_MENU program has finished updating the menu file. '
      WRITE(*,*) ' '
      WRITE(*,*) "###################################################################"
      WRITE(*,*)
      WRITE(*,*) debugSTAT, ' End Date of log  -> ', DATEEND
      WRITE(*,*) debugSTAT, ' End Time of log  -> ', TIMEEND
      WRITE(*,*) debugSTAT, ' ELAPSED TIME : ', REAL(COUNT_1 - COUNT_0)/ REAL(COUNT_RATE)
      WRITE(*,*)
      WRITE(*,*) 'END OF PROGRAM. '
      WRITE(12,*) "###################################################################"
      WRITE(12,*) ' '
      WRITE(12,*) '   The ACRU_MENU program has finished updating the menu file. '
      WRITE(12,*) ' '
      WRITE(12,*) "###################################################################"
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' End Date of log  -> ', DATEEND
      WRITE(12,*) debugSTAT, ' End Time of log  -> ', TIMEEND
      WRITE(12,*) debugSTAT, ' ELAPSED TIME : ', REAL(COUNT_1 - COUNT_0)/ REAL(COUNT_RATE)
      WRITE(12,*)
      WRITE(12,*) 'END OF PROGRAM. '
      CLOSE(12)
      STOP
END PROGRAM ACRU_MENU_CALIBRATION
