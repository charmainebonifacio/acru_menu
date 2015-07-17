!###################################################################
! TITLE        : ACRU_MENU
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE REVISED : November 25, 2014
!-------------------------------------------------------------------
! DESCRIPTION  : This is a test run for implementing the logfile
!                system.
! REQUIREMENT  : MUST run the .EXE file within the input directory.
! INPUT        : 1) MENU FILE = MENU
!                2) VARIABLE FILE = menu_variable.txt
! OUTPUT       : 1) LOG File
!                2) Updated Menu File
!###################################################################
PROGRAM ACRU_MENU
IMPLICIT NONE
CHARACTER(LEN=10), PARAMETER :: debugSTAT = '[ STATUS ] '
CHARACTER(LEN=10), PARAMETER :: debugRES = '[ RESULT ] '
CHARACTER(LEN=10), PARAMETER :: debugASK = '[  ASK  ] '
INTEGER :: LINE,I,J,K,L,M,N,O,P,OK,IOcheck
INTEGER :: ISUBNO,LINECOIAM,LINECAY,LINEELAIM,LINEROOTA
INTEGER :: LINEICC,LINEALBEDO,ICONS,ISWAVE
INTEGER :: MINSUB,MAXSUB,LOOPBK
REAL :: D1, D2
REAL, DIMENSION(12) :: COIAM, CAY, ELAIM, ROOTA, ICC, ALBEDO
CHARACTER(LEN=4), PARAMETER :: MENU = 'MENU'
CHARACTER(LEN=50), PARAMETER :: MENUVARS = "menu_variable.txt"
CHARACTER(LEN=200) :: OUTFILE, INFILE, VARFILE, TMPFILE, LOGRUN
CHARACTER(LEN=200) :: MENUCOPY, MENUTMP, DIR, COPYFILE, KILLFILE
CHARACTER(LEN=80) :: DUMMY
CHARACTER(LEN=8) :: DATEINFO
CHARACTER(LEN=4) :: YEAR, MONTH*2, DAY*2
CHARACTER(LEN=2) :: HRS, MIN, SEC*6
CHARACTER(LEN=10) :: DATE, TIMEINFO, TIMENOW*12, DATENOW
CHARACTER(LEN=*), PARAMETER:: FORMFILE = "( A70 )"
CHARACTER(LEN=*), PARAMETER:: FORMX = "( 3X,I4 )"
CHARACTER(LEN=*), PARAMETER:: FORMY = "( A200 )"
CHARACTER(LEN=*), PARAMETER:: FORMZ = "( 66X,I1,5X,I1 )"
CHARACTER(LEN=*), PARAMETER:: FORMA = "( 1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4 )"
LOGICAL :: EX
!***********************************************************************
! Setup new MENU file
!***********************************************************************
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
! USER INPUT
!***********************************************************************
	  WRITE(*,*) debugSTAT, ' Date of log  -> ', DATENOW
      WRITE(*,*) debugSTAT, ' Time of log  -> ', TIMENOW
      WRITE(*,*)
      LOGRUN = 'LOGRUN_MENU_'//DATE//'.txt'
      INQUIRE(FILE=LOGRUN, EXIST=EX)
      WRITE(*,*) debugSTAT, ' Checking file: ', LOGRUN
      IF (EX) THEN
        OPEN(UNIT=12,FILE=LOGRUN,STATUS='REPLACE',IOSTAT=OK)
      ELSE
        OPEN(UNIT=12,FILE=LOGRUN,STATUS='NEW',IOSTAT=OK)
      ENDIF
!     IF (OK/=0) THEN
!       WRITE(*,*) debugRES, 'COULD NOT OPEN FILE.'
!       STOP
!     ENDIF
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
      VARFILE = MENUVARS
	    OPEN(UNIT=10,FILE=VARFILE,IOSTAT=OK)
	    WRITE(*,*) debugRES, ' File opened: ', VARFILE
	    WRITE(*,*) debugRES, ' File status ok = ', OK
      WRITE(12,*) debugSTAT, ' VARIABLE FILE -> ', VARFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      OUTFILE = MENU
	    OPEN(UNIT=20,FILE=OUTFILE,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', OUTFILE
	    WRITE(*,*) debugRES, ' File status ok = ', OK
      WRITE(12,*) debugSTAT, ' MENUFILE -> ', OUTFILE
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      MENUCOPY = MENU//'_OLD'
      CALL SYSTEM( "copy " // OUTFILE // " " // MENUCOPY)
      OPEN(UNIT=30,FILE=MENUCOPY,IOSTAT=OK)
      WRITE(*,*) debugRES, ' File opened: ', MENUCOPY
	    WRITE(*,*) debugRES, ' File status ok = ', OK
      WRITE(12,*) debugSTAT, ' MENUFILE COPY -> ', MENUCOPY
      WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      CLOSE(10)
  999 CLOSE(30)
      CLOSE(20)
!***********************************************************************
! END PROGRAM
!***********************************************************************
      WRITE(12,*) "###################################################################"
      WRITE(12,*) ' '
      WRITE(12,*) '   The ACRU_MENU program has finished creating a new menu file. '
      WRITE(12,*) ' '
      WRITE(12,*) "###################################################################"
      WRITE(12,*)
      WRITE(12,*) 'END OF PROGRAM. '
      CLOSE(12)
      WRITE(*,*) "###################################################################"
      WRITE(*,*) ' '
      WRITE(*,*) '   The ACRU_MENU program has finished creating a new menu file. '
      WRITE(*,*) ' '
      WRITE(*,*) "###################################################################"
      WRITE(*,*)
      WRITE(*,*) 'END OF PROGRAM. '
   	  STOP
END PROGRAM ACRU_MENU
