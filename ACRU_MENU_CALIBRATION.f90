!###################################################################
! MODULE TITLE : M_SYSTEMCHECKS
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 27, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain DATE and TIME subroutines.
! SUBROUTINE 1 : This subroutine will calculate the date and time.
! SUBROUTINE 2 : This subroutine will check if the file was opened
!                successfully.
! SUBROUTINE 3 : This subroutine will check ISUBNO value.
! SUBROUTINE 4 : This subroutine will calculate the elapsed time
!###################################################################
MODULE M_SYSTEMCHECKS
IMPLICIT NONE

CONTAINS
   SUBROUTINE DATETIMELOG(DATE, DATENOW, TIMENOW)
   CHARACTER(LEN=8) :: DATEINFO
   CHARACTER(LEN=4) :: YEAR, MONTH*2, DAY*2
   CHARACTER(LEN=2) :: HRS, MIN, SEC*6
   CHARACTER(LEN=10) :: TIMEINFO
   CHARACTER(LEN=10), INTENT(OUT) :: DATE, DATENOW
   CHARACTER(LEN=12), INTENT(OUT) :: TIMENOW
      CALL DATE_AND_TIME(DATEINFO, TIMEINFO)
      YEAR = DATEINFO(1:4)
      MONTH = DATEINFO(5:6)
      DAY = DATEINFO(7:8)
      DATE = YEAR // '_' // MONTH // '_' // DAY
      DATENOW = YEAR // '-' // MONTH // '-' // DAY
      HRS = TIMEINFO(1:2)
      MIN = TIMEINFO(3:4)
      SEC = TIMEINFO(5:10)
      TIMENOW = HRS // ':' // MIN // ':' // SEC
   END SUBROUTINE DATETIMELOG
   SUBROUTINE FILESTATCHECK(STATUS, UNIT_NO)
   INTEGER, INTENT(IN) :: UNIT_NO, STATUS
      IF (STATUS==0) THEN
        WRITE(UNIT_NO,*) 'SUCCESSFULLY OPENED FILE.'
      END IF
      IF (STATUS/=0) THEN
        WRITE(UNIT_NO,*) 'COULD NOT OPEN FILE.'
      END IF
   END SUBROUTINE FILESTATCHECK

   SUBROUTINE VALUECHECK(VALUE, UNIT_NO1, UNIT_NO2)
   INTEGER, INTENT(IN) :: UNIT_NO1, UNIT_NO2, VALUE
      IF (VALUE==0) THEN
        WRITE(UNIT_NO1,*)
        WRITE(UNIT_NO1,*) "###################################################################"
        WRITE(UNIT_NO1,*)
        WRITE(UNIT_NO1,*) 'INVALID VALUE. TERMINATING PROGRAM'
        WRITE(UNIT_NO1,*)
        CLOSE(UNIT_NO1)
        CLOSE(UNIT_NO2)
        STOP ' INVALID VALUE. TERMINATING PROGRAM. '
      END IF
   END SUBROUTINE VALUECHECK

   SUBROUTINE ELAPSEDTIME(ELAPSED_TIME, SYS_COUNT_0, SYS_COUNT_1, COUNTRATE)
   INTEGER, INTENT(IN) :: SYS_COUNT_0, SYS_COUNT_1, COUNTRATE
   REAL, INTENT(OUT) :: ELAPSED_TIME
      ELAPSED_TIME = 0
      ELAPSED_TIME = REAL(SYS_COUNT_1 - SYS_COUNT_0)/ REAL(COUNTRATE)
   END SUBROUTINE ELAPSEDTIME
END MODULE M_SYSTEMCHECKS

!###################################################################
! MODULE TITLE : M_LOGSYSTEM
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 27, 2015
! DATE REVISED : July 27, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed for the LOG FILE to work.
! SUBROUTINE 1 : This subroutine will print out the start log
!                header for this script.
! SUBROUTINE 2 : This subroutine will print out the end log
!                header for this script.
!###################################################################
MODULE M_LOGSYSTEM
IMPLICIT NONE

CONTAINS
   SUBROUTINE STARTPROGRAMLOG(UNIT_NO)
   INTEGER, INTENT(IN) :: UNIT_NO
      WRITE(UNIT_NO,*)
      WRITE(UNIT_NO,*) 'START OF PROGRAM. '
      WRITE(UNIT_NO,*)
      WRITE(UNIT_NO,*) "###################################################################"
      WRITE(UNIT_NO,*) ' '
      WRITE(UNIT_NO,*) ' The ACRU_MENU program will COPY values from a tab-delimited file. '
      WRITE(UNIT_NO,*) ' '
      WRITE(UNIT_NO,*) "###################################################################"
      WRITE(UNIT_NO,*)
   END SUBROUTINE STARTPROGRAMLOG

   SUBROUTINE ENDPROGRAMLOG(UNIT_NO)
   INTEGER, INTENT(IN) :: UNIT_NO
      WRITE(UNIT_NO,*)
      WRITE(UNIT_NO,*) "###################################################################"
      WRITE(UNIT_NO,*) ' '
      WRITE(UNIT_NO,*) '   The ACRU_MENU program has finished updating the menu file. '
      WRITE(UNIT_NO,*) ' '
      WRITE(UNIT_NO,*) "###################################################################"
      WRITE(UNIT_NO,*)
      WRITE(UNIT_NO,*) 'END OF PROGRAM. '
   END SUBROUTINE ENDPROGRAMLOG
END MODULE M_LOGSYSTEM

!###################################################################
! MODULE TITLE : M_CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 27, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed for the program to work.
! SUBROUTINE 1 : This subroutine will calculate the row line
!                associated with the variable.
!###################################################################
MODULE M_CALIBRATION
IMPLICIT NONE

CONTAINS
   SUBROUTINE CALCVARLINE(LINE_VAR, ISUBNO, VAR_ROW)
   INTEGER, INTENT(OUT) :: LINE_VAR
   INTEGER, INTENT(IN) :: ISUBNO, VAR_ROW
      LINE_VAR = 0
      LINE_VAR = 23 + (ISUBNO + 5) * VAR_ROW
   END SUBROUTINE CALCVARLINE
END MODULE M_CALIBRATION
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
!                COIAM and ICC.
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! INPUT        : 1) MENU FILE = MENU
!                2) VARIABLE FILE = menu_variable.txt
! OUTPUT       : 1) Updated MENU File
!                2) LOG file
!###################################################################
PROGRAM P_ACRU_MENU_CALIBRATION
USE M_SYSTEMCHECKS
USE M_LOGSYSTEM
USE M_CALIBRATION
IMPLICIT NONE

CHARACTER(LEN=11), PARAMETER :: debugSTAT = '[ STATUS ] '
CHARACTER(LEN=11), PARAMETER :: debugRES = '[ RESULT ] '
CHARACTER(LEN=4), PARAMETER :: MENU = 'MENU'
CHARACTER(LEN=30), PARAMETER :: MENUVARS = 'menu_variable.txt'
CHARACTER(LEN=30) :: OUTFILE, INFILE, LOGRUN, VARFILE
CHARACTER(LEN=80) :: DUM, DUM2, MSG
CHARACTER(LEN=10) :: DATE, DATENOW, DATEEND
CHARACTER(LEN=12) :: TIMENOW, TIMEEND
INTEGER :: ISUBNO
INTEGER :: COUNT_0, COUNT_1, COUNT_RATE, COUNT_MAX
INTEGER :: LINE, I, L, P, OK, TOTALLINE
INTEGER :: LINECOIAM, LINECAY, LINEELAIM, LINEROOTA, LINEEOF
INTEGER :: LINEICC, LINEALBEDO, ICONS, ISWAVE
LOGICAL :: EX
REAL :: D1, D2, ELAPSED_TIME
INTEGER, DIMENSION(12) :: ICC
REAL, DIMENSION(12) :: COIAM, CAY, ELAIM, ROOTA, ALBEDO

!***********************************************************************
! SETUP START TIME
!***********************************************************************
      CALL SYSTEM_CLOCK(COUNT_0, COUNT_RATE, COUNT_MAX)
      CALL DATETIMELOG(DATE, DATENOW, TIMENOW)
!***********************************************************************
! START PROGRAM
!***********************************************************************
      LOGRUN = 'LOGRUN_MENU_'//DATE//'.txt'
      INQUIRE(FILE=LOGRUN, EXIST=EX)
      WRITE(*,*) debugSTAT, ' Checking file: ', LOGRUN
      IF (EX) THEN
        OPEN(UNIT=12,FILE=LOGRUN,STATUS='REPLACE',IOSTAT=OK)
      ELSE
        OPEN(UNIT=12,FILE=LOGRUN,STATUS='NEW',IOSTAT=OK)
      ENDIF
!***********************************************************************
! FORMAT
!***********************************************************************
  106 FORMAT(1X, A11, A20, F10.5)
  107 FORMAT(1X, A11, A20, A30)
  108 FORMAT(1X, A11, A20, A12)
  109 FORMAT(1X, A11, '  FILENAME OPENED : ', A30)
  110 FORMAT(1X, A11, '      FILE STATUS : ', I4)
  111 FORMAT(A80)
  112 FORMAT(3X,I4)
  113 FORMAT(1X, A11, A30, I7)
!  114 FORMAT(1X, A11, A15, I7)
  115 FORMAT(1X, A11, A20, I7)
  116 FORMAT(1X, A11, A50, I7)
!  117 FORMAT(10X, A11, A20, I7)
  118 FORMAT(10X, A11, ' CALIBRATED LINE ', I7, ' --- HRU # ', I4, ' OUT OF ', I4)
  119 FORMAT(10X,A11,A17,I7,A9,I4,A8,I4)
  120 FORMAT(1X, A11, ' PROCESSED ', I7, ' NUMBER OF LINES.')
!***********************************************************************
! START LOG
!***********************************************************************
      CALL STARTPROGRAMLOG(12)
      WRITE(12,108) debugSTAT, '             DATE : ', DATENOW
      WRITE(12,108) debugSTAT, '             TIME : ', TIMENOW
      WRITE(12,*)
      WRITE(12,107) debugSTAT, '          LOGFILE : ', LOGRUN
      WRITE(12,110) debugSTAT, OK
      WRITE(12,*)
      WRITE(12,*) '[ P R O C E S S I N G   M E N U   F I L E ] '
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' Copy MENU file and rename to MENU_OLD. '
      INFILE = MENU
      OUTFILE = MENU//'_OLD'
      CALL SYSTEM( "copy " // INFILE // " " // OUTFILE)
      CALL SYSTEM( "copy " // INFILE // " " // 'ORIGINAL_'//MENU)
      WRITE(12,*)
      WRITE(12,*) '[ P R O C E S S I N G   R E Q U I R E D   F I L E S ] '
      WRITE(12,*)
      VARFILE = MENUVARS
      OPEN(UNIT=11,FILE=VARFILE,IOSTAT=OK)
      WRITE(12,*) '>> Variable File.'
      CALL FILESTATCHECK(OK,12)
      WRITE(12,109) debugRES, VARFILE
      WRITE(12,110) debugSTAT, OK
      WRITE(12,*)
      OPEN(UNIT=20,FILE=OUTFILE,IOSTAT=OK)
      WRITE(12,*) '>> Old copy of Menu File.'
      CALL FILESTATCHECK(OK,12)
      WRITE(12,109) debugRES, OUTFILE
      WRITE(12,110) debugSTAT, OK
      WRITE(12,*)
      OPEN(UNIT=30,FILE=INFILE,IOSTAT=OK)
      WRITE(12,*) '>> Working copy of Menu File.'
      CALL FILESTATCHECK(OK,12)
      WRITE(12,109) debugRES, INFILE
      WRITE(12,110) debugSTAT, OK
!***********************************************************************
! START PROCESSING MENU FILE - How many HRUs in this menu file?
!***********************************************************************
      ISUBNO=0
      P=1
      DO 898 WHILE (P.LT.11)
        READ(20,111) DUM2
        P=P+1
  898 END DO
      READ(20,112) ISUBNO
      CLOSE(20)
      WRITE(12,*)
      WRITE(12,*) '[ O B T A I N   N U M B E R   O F   H R U  ] '
      WRITE(12,*)
      WRITE(12,113) debugRES, '       NUMBER OF HRU IN MENU : ', ISUBNO
      CALL VALUECHECK(ISUBNO,12,30)
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
      WRITE(12,*)
      WRITE(12,*) '[ E O F   C H E C K ] '
      WRITE(12,*)
      WRITE(12,113) debugRES, '  COUNTED END OF FILE LINES : ', TOTALLINE
      WRITE(12,113) debugRES, '    CALCULATED LINES BY HRU : ', LINEEOF
!***********************************************************************
! Calculate line number for each variable!
! Then overwrite values once line is found. Continue for X HRUs.
!***********************************************************************
      CALL CALCVARLINE(LINEALBEDO, ISUBNO, 28)
      CALL CALCVARLINE(LINECAY, ISUBNO, 53)
      CALL CALCVARLINE(LINEELAIM, ISUBNO, 54)
      CALL CALCVARLINE(LINEROOTA, ISUBNO, 56)
      CALL CALCVARLINE(LINECOIAM, ISUBNO, 67)
      CALL CALCVARLINE(LINEICC, ISUBNO, 141)
      WRITE(12,*)
      WRITE(12,*) '[ S U M M A R Y   O F   V A R I A B L E S ] '
      WRITE(12,*)
      WRITE(12,115) debugSTAT,' ALBEDO Line = ',LINEALBEDO
      WRITE(12,115) debugSTAT,'    CAY Line = ',LINECAY
      WRITE(12,115) debugSTAT,'  ELAIM Line = ',LINEELAIM
      WRITE(12,115) debugSTAT,'  ROOTA Line = ',LINEROOTA
      WRITE(12,115) debugSTAT,'  COIAM Line = ',LINECOIAM
      WRITE(12,115) debugSTAT,'    ICC Line = ',LINEICC
      OPEN(UNIT=20,FILE=OUTFILE)
      LINE=1
      WRITE(12,*)
      WRITE(12,*) '[ C A L I B R A T I N G   M E N U   F I L E ] '
      WRITE(12,*)
      DO 900 WHILE (LINE.LT.LINEEOF)
        L=1
!=================================================
! CHECK WHERE VARIABLES SHOULD BE OVERWRITTEN
        IF(LINE.EQ.LINEALBEDO) THEN ! CHECK WHERE ALBEDO SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,116) debugSTAT, ' .... ALBEDO CALIBRATION STARTING FROM LINE >> ', LINE
          DO 901 WHILE (L.LE.ISUBNO)
            WRITE(12,115) debugSTAT,' Processing Line >> ', LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,220)ICONS,ISWAVE ! Read original MENU ICONS and ISWAVE variables
  220       FORMAT(66X,I1,5X,I1)
            WRITE(12,221)(ALBEDO(I),I=1,12),ICONS,ISWAVE,L
            WRITE(30,221)(ALBEDO(I),I=1,12),ICONS,ISWAVE,L
  221       FORMAT(1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4)
            WRITE(12,119) debugRES,' CALIBRATED LINE ', LINE,'--- HRU # ',L,' OUT OF ',ISUBNO
            L=L+1
	        LINE=LINE+1
  901     END DO
          CLOSE(11)
        ELSEIF(LINE.EQ.LINECAY) THEN ! CHECK WHERE CAY SHOULD BE OVERWRITTEN
          OPEN(UNIT=11,FILE=VARFILE)
          READ(11,*) DUM2  ! Header
          READ(11,*) DUM2  ! Header
          WRITE(12,*) '================================================================================='
          WRITE(12,116) debugSTAT, ' .... CAY CALIBRATION STARTING FROM LINE >> ', LINE
          DO 902 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,115) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(CAY(I),I=1,12),(L)
            WRITE(30,222)(CAY(I),I=1,12),(L)
  222       FORMAT(1X,12(F4.2,1X),15X,I4)
            WRITE(12,118) debugRES,LINE,L,ISUBNO
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
          WRITE(12,116) debugSTAT, ' .... ELAIM CALIBRATION STARTING FROM LINE >> ', LINE
          DO 903 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,115) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(ELAIM(I),I=1,12),(L)
            WRITE(30,222)(ELAIM(I),I=1,12),(L)
            WRITE(12,118) debugRES,LINE,L,ISUBNO
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
          WRITE(12,116) debugSTAT, ' .... ROOTA CALIBRATION STARTING FROM LINE >> ', LINE
          DO 904 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,115) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,222)(ROOTA(I),I=1,12),(L)
            WRITE(30,222)(ROOTA(I),I=1,12),(L)
            WRITE(12,118) debugRES,LINE,L,ISUBNO
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
          WRITE(12,116) debugSTAT, ' .... COIAM CALIBRATION STARTING FROM LINE >> ', LINE
          DO 905 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,115) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
	        WRITE(12,222)(COIAM(I),I=1,12),(L)
  	        WRITE(30,222)(COIAM(I),I=1,12),(L)
            WRITE(12,118) debugRES,LINE,L,ISUBNO
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
          WRITE(12,116) debugSTAT, ' .... ICC CALIBRATION STARTING FROM LINE >> ', LINE
          DO 906 WHILE (L.LE.ISUBNO)
            WRITE(*,*) debugSTAT, ' Processing Line >> ',LINE
            WRITE(12,115) debugSTAT, ' Processing Line >> ',LINE
            READ(11,*)D1,D2, &
              (COIAM(I),I=1,12),(CAY(I),I=1,12), &
              (ELAIM(I),I=1,12),(ROOTA(I),I=1,12), &
              (ICC(I),I=1,12),(ALBEDO(I),I=1,12)
            READ(20,111) DUM
            WRITE(12,223)(ICC(I),I=1,12),(L)
            WRITE(30,223)(ICC(I),I=1,12),(L)
  223       FORMAT(2X,12(I3.2,2X),14X,I4)
  		    WRITE(12,119) debugRES,' CALIBRATED LINE ', LINE,'--- HRU # ',L,' OUT OF ',ISUBNO
            L=L+1
	        LINE=LINE+1
  906     END DO
          WRITE(12,*) '================================================================================='
          CLOSE(11)
       ELSE ! SIMPLY READ AND COPY LINES
          READ(20,111) DUM
          WRITE(30,111) DUM
          LINE=LINE+1
        ENDIF
  900 END DO
      CLOSE(20)
      WRITE(12,*)
      WRITE(*,120) debugSTAT, LINE
      WRITE(12,120) debugSTAT, LINE
      WRITE(12,*)
      MSG = ' MENU CALIBRATED & CREATED BY CHARMAINE BONIFACIO. MENU SCRIPT VERSION --- '
      WRITE(30,111) MSG, DATE, ' | ', TIMENOW
 125  FORMAT(A75,A10,A3,A5)
      ENDFILE(30)
      CLOSE(30)
!***********************************************************************
! ELAPSED TIME
!***********************************************************************
      CALL SYSTEM_CLOCK(COUNT_1, COUNT_RATE, COUNT_MAX)
      CALL DATETIMELOG(DATE, DATEEND, TIMEEND)
!***********************************************************************
! END PROGRAM
!***********************************************************************
      WRITE(12,108) debugSTAT, '             DATE : ', DATEEND
      WRITE(12,108) debugSTAT, '             TIME : ', TIMEEND
      CALL ELAPSEDTIME(ELAPSED_TIME, COUNT_0, COUNT_1, COUNT_RATE)
      WRITE(12,106) debugSTAT, '     ELAPSED TIME : ', ELAPSED_TIME
      CALL STARTPROGRAMLOG(12)
      CLOSE(12)
END PROGRAM P_ACRU_MENU_CALIBRATION
