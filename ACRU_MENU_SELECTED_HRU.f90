!###################################################################
! TITLE        : ACRU_MENU_SELECTED_HRU
!-------------------------------------------------------------------
! CREATED BY    : Dr. Stefan W. Kienzle
! DATE EDITED  : Otober 9, 2009
! REVISED BY   : Charmaine Bonifacio
! DATE REVISED : May 11, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The program will read a MENU file and selects only
!                those HRUs based on lowest and highest HRUs.
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! INPUT        : 1) Lowest HRU Number
!                2) Highest HRU Number
! OUTPUT       : 1) New MENU File
!###################################################################
PROGRAM ACRU_MENU_SELECTED_HRU
IMPLICIT NONE
CHARACTER(LEN=11), PARAMETER :: debugSTAT = '[ STATUS ] '
CHARACTER(LEN=11), PARAMETER :: debugRES = '[ RESULT ] '
CHARACTER(LEN=11), PARAMETER :: debugASK = '[  ASK  ] '
INTEGER :: OK
INTEGER :: HRUNum, HRUFirst, HRULast
INTEGER :: COUNT_0, COUNT_1, COUNT_RATE, COUNT_MAX, COUNTER, COUNT2
CHARACTER(LEN=4), PARAMETER :: MENU = 'MENU'
CHARACTER(LEN=200) :: OUTFILE, INFILE, LOGRUN
CHARACTER(LEN=129) :: LINE
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
      WRITE(*,*) '     The ACRU_MENU program will create a revised MENU File with . '
      WRITE(*,*) '     a subset of the original number of HRUs. '
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
      WRITE(12,*) '     The ACRU_MENU program will create a revised MENU File with . '
      WRITE(12,*) '     a subset of the original number of HRUs. '
      WRITE(12,*) ' '
      WRITE(12,*) "###################################################################"
      WRITE(12,*)
	    WRITE(12,*) debugSTAT, ' DATE -> ', DATENOW
      WRITE(12,*) debugSTAT, ' TIME -> ', TIMENOW
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' LOGFILE -> ', LOGRUN
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      INFILE = MENU
	    OPEN(UNIT=20,FILE=INFILE,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', INFILE
	    WRITE(*,*) debugRES, ' File status ok = ', OK
      WRITE(12,*) debugSTAT, ' MENUFILE -> ', INFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      OUTFILE = MENU//'_SELECTED_HRU'
      OPEN(UNIT=30,FILE=OUTFILE,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', OUTFILE
	    WRITE(*,*) debugRES, ' File status ok = ', OK
      WRITE(12,*) debugSTAT, ' MENUFILE COPY -> ', OUTFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      WRITE(*,*)
!***********************************************************************
! START USER INPUT
!***********************************************************************
      WRITE(*,*) "------------------------------------------------------"
      WRITE(*,*) "     Key in the first HRU number to process : "
      READ(*,*) HRUFirst
      WRITE(*,*) 'HRUNum = ', HRUFirst
      WRITE(*,*) "------------------------------------------------------"
      WRITE(12,*) "------------------------------------------------------"
      WRITE(12,*) "     Key in the first HRU number to process : "
      WRITE(12,*) 'HRUNum = ', HRUFirst
      WRITE(12,*) "------------------------------------------------------"
!***********************************************************************
! CONTINUE USER INPUT
!***********************************************************************
      WRITE(*,*) "------------------------------------------------------"
      WRITE(*,*) "     Key in the last HRU number to process : "
      READ(*,*) HRULast
      WRITE(*,*) 'HRUNum = ', HRULast
      WRITE(*,*) "------------------------------------------------------"
      WRITE(12,*) "------------------------------------------------------"
      WRITE(12,*) "     Key in the last HRU number to process : "
      WRITE(12,*) 'HRUNum = ', HRULast
      WRITE(12,*) "------------------------------------------------------"
!***********************************************************************
! CONTINUE USER INPUT
!***********************************************************************
      WRITE(*,*) "------------------------------------------------------"
      WRITE(*,*) "     Key in the total HRU number in the MENU : "
      READ(*,*) HRUNum
      WRITE(*,*) 'HRUNum = ', HRUNum
      WRITE(*,*) "------------------------------------------------------"
      WRITE(12,*) "------------------------------------------------------"
      WRITE(12,*) "     Key in the total HRU number in the MENU : "
      WRITE(12,*) 'HRUNum = ', HRUNum
      WRITE(12,*) "------------------------------------------------------"
!***********************************************************************
! START PROCESSING FILE
!***********************************************************************
      COUNT2 = 1
	    COUNTER = 0
  100 FORMAT(A80)
!     Copy first 18 lines
      DO 700 WHILE (COUNTER.LT.17)
	      COUNTER = COUNTER + 1
        READ(20,100,END=999)LINE
        WRITE(30,100)LINE
  700 CONTINUE
!     Proceed with the rest of the MENU file but write only selected HRUs
      DO 800 WHILE (COUNT2.LT.147)
         COUNTER = 0
         READ(20,100,END=999)LINE
         WRITE(30,100)LINE
         READ(20,100,END=999)LINE
         WRITE(30,100)LINE
         READ(20,100,END=999)LINE
         WRITE(30,100)LINE
         READ(20,100,END=999)LINE
         WRITE(30,100)LINE
         READ(20,100,END=999)LINE
         DO 801 WHILE (COUNTER.LT.HRUNum)
            COUNTER = COUNTER + 1
            READ(20,100,END=999)LINE
            IF(COUNTER.GE.HRUFirst.AND.COUNTER.LE.HRULast) THEN
              WRITE(30,100)LINE
            ENDIF
  801    CONTINUE
         WRITE(*,*)'Processed ACRU MENU ITEM ',COUNT2,' out of ',HRUNum
         WRITE(12,*)'Processed ACRU MENU ITEM ',COUNT2,' out of ',HRUNum
         COUNT2 = COUNT2 + 1
  800 CONTINUE
  999 WRITE(*,*) '****************************************************'
      CLOSE(30)
      CLOSE(20)
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
      WRITE(*,*) '   The ACRU_MENU program has finished creating a new menu file. '
      WRITE(*,*) ' '
      WRITE(*,*) "###################################################################"
      WRITE(*,*)
      WRITE(*,*) debugSTAT, ' End Date of log  -> ', DATEEND
      WRITE(*,*) debugSTAT, ' End Time of log  -> ', TIMEEND
      WRITE(*,*)
      WRITE(*,*) debugSTAT, ' Time Elapsed : ', REAL(COUNT_1 - COUNT_0)/ REAL(COUNT_RATE)
      WRITE(*,*)
      WRITE(*,*) 'END OF PROGRAM. '
      WRITE(12,*) "###################################################################"
      WRITE(12,*) ' '
      WRITE(12,*) '   The ACRU_MENU program has finished creating a new menu file. '
      WRITE(12,*) ' '
      WRITE(12,*) "###################################################################"
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' End Date of log  -> ', DATEEND
      WRITE(12,*) debugSTAT, ' End Time of log  -> ', TIMEEND
      WRITE(12,*)
      WRITE(12,*) debugSTAT, ' Time Elapsed : ', REAL(COUNT_1 - COUNT_0)/ REAL(COUNT_RATE)
      WRITE(12,*)
      WRITE(12,*) 'END OF PROGRAM. '
      CLOSE(12)
   	  STOP
END PROGRAM ACRU_MENU_SELECTED_HRU
