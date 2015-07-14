      !###################################################################
      ! TITLE        : ACRU_MENU_CALIBRATION
      !-------------------------------------------------------------------
      ! CREATED BY    : Dr. Stefan W. Kienzle
      ! DATE CREATED  : May 19, 2008                   
      ! REVISED BY   : Charmaine Bonifacio
      ! DATE REVISED : June 29, 2015
      !-------------------------------------------------------------------
      ! DESCRIPTION  : The program will copy values from a tab delimited
      !                file that contains COIAM, CAY, ELAIM, ROOTA, ICC,
      !                Albedo.
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
      CHARACTER(LEN=11), PARAMETER :: debugASK = '[  ASK  ] '
      INTEGER :: ISUBNO
      INTEGER :: COUNT_0, COUNT_1, COUNT_RATE, COUNT_MAX
      INTEGER :: LINE,I,L,P,OK
      INTEGER :: LINECOIAM,LINECAY,LINEELAIM,LINEROOTA
      INTEGER :: LINEICC,LINEALBEDO,ICONS,ISWAVE
      REAL :: D1, D2
      REAL, DIMENSION(12) :: COIAM, CAY, ELAIM, ROOTA, ICC, ALBEDO
      CHARACTER(LEN=4), PARAMETER :: MENU = 'MENU'
      CHARACTER(LEN=50), PARAMETER :: MENUVARS = "menu_variable.txt"
      CHARACTER(LEN=200) :: OUTFILE, INFILE, LOGRUN, VARFILE
      CHARACTER(LEN=129) :: DUM
      CHARACTER(LEN=8) :: DATEINFO
      CHARACTER(LEN=4) :: YEAR, MONTH*2, DAY*2
      CHARACTER(LEN=2) :: HRS, MIN, SEC*6
      CHARACTER(LEN=10) :: DATE, TIMEINFO, TIMENOW*12, DATENOW, TIMEEND*12, DATEEND
      CHARACTER(LEN=*), PARAMETER:: FORMFILE = "( A70 )"
      CHARACTER(LEN=*), PARAMETER:: FORMX = "( 3X,I4 )"
      CHARACTER(LEN=*), PARAMETER:: FORMY = "( A200 )"
      CHARACTER(LEN=*), PARAMETER:: FORMZ = "( 66X,I1,5X,I1 )"
      CHARACTER(LEN=*), PARAMETER:: FORMA = "( 1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4 )"
      LOGICAL :: EX
      !***********************************************************************
      ! Setup new MENU file
      !*******************************************************gi****************
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
      	  WRITE(*,*) debugRES, ' File status ok = ', OK
            WRITE(12,*) debugSTAT, ' VARIABLE FILE -> ', VARFILE
            WRITE(12,*) debugSTAT, ' STATUS -> ', OK
            OPEN(UNIT=20,FILE=OUTFILE,IOSTAT=OK)
            WRITE(*,*) debugRES, ' File opened: ', OUTFILE
      	  WRITE(*,*) debugRES, ' File status ok = ', OK
            WRITE(12,*) debugSTAT, ' MENUFILE COPY -> ', OUTFILE
            WRITE(12,*) debugSTAT, ' STATUS -> ', OK
            OPEN(UNIT=30,FILE=INFILE,IOSTAT=OK)
            WRITE(*,*) debugRES, ' File opened: ', INFILE
      	  WRITE(*,*) debugRES, ' File status ok = ', OK
            WRITE(12,*) debugSTAT, ' MENUFILE -> ', INFILE
            WRITE(12,*) debugSTAT, ' STATUS -> ', OK
      !***********************************************************************
      ! START PROCESSING MENU FILE
      !***********************************************************************
            ISUBNO=0
        111 FORMAT(A200)
            DO P=1,11
              READ(20,111) DUM
            END DO
            READ(20,112) ISUBNO
            WRITE(12,112) ISUBNO
        112 FORMAT(3X,I4)
            CLOSE(20)
            WRITE(*,*) 'Number of HRUs = ',ISUBNO
            WRITE(12,*) 'Number of HRUs = ',ISUBNO
            OPEN(UNIT=20,FILE=OUTFILE)
      	  LINEALBEDO=23+((ISUBNO+5)*28)
            LINECAY   =23+((ISUBNO+5)*53)
            LINEELAIM =23+((ISUBNO+5)*54)
      	  LINEROOTA =23+((ISUBNO+5)*56)
            LINECOIAM =23+((ISUBNO+5)*67)
      	  LINEICC   =23+((ISUBNO+5)*141)
            LINE=1
            DO 900 WHILE (LINE.LT.10000000)
              L=1
      	    READ(20,111,END=999)DUM
      !     CHECK WHERE ALBEDO SHOULD BE OVERWRITTEN  =================================
              IF(LINE.EQ.LINEALBEDO) THEN
                OPEN(UNIT=11,FILE=VARFILE)
                READ(11,111)DUM
      	      READ(11,111)DUM
                DO 901 WHILE (L.LE.ISUBNO)
                  READ(11,*)D1,D2, &
                    (COIAM(I),I=1,20),(CAY(I),I=1,20), &
                    (ELAIM(I),I=1,20),(ROOTA(I),I=1,20), &
                    (ICC(I),I=1,20),(ALBEDO(I),I=1,20)
      !      read original MENU ICONS and ISWAVE variables
                  READ(20,113)ICONS,ISWAVE
        113       FORMAT(66X,I1,5X,I1)
      	        WRITE(30,222)(ALBEDO(I),I=1,20),ICONS,ISWAVE,L
        222       FORMAT(1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4)
                  L=L+1
      	        LINE=LINE+1
                  WRITE(*,*) 'Processing Line ',LINE, 'Overwriting ALBEDO'
                  WRITE(12,*) 'Processing Line ',LINE, 'Overwriting ALBEDO'
        901     CONTINUE
                CLOSE(11)
              ENDIF
      !     END CHECK WHERE ALBEDO SHOULD BE OVERWRITTEN   ===========================
      !     CHECK WHERE COIAM SHOULD BE OVERWRITTEN   =========================
              IF(LINE.EQ.LINECOIAM) THEN
                OPEN(UNIT=11,FILE=VARFILE)
                READ(11,111)DUM
      	      READ(11,111)DUM
                DO 902 WHILE (L.LE.ISUBNO)
                  READ(11,*)D1,D2, &
                     (COIAM(I),I=1,20),(CAY(I),I=1,20), &
                     (ELAIM(I),I=1,20),(ROOTA(I),I=1,20), &
                     (ICC(I),I=1,20),(ALBEDO(I),I=1,20)
                  READ(20,111)DUM
      	        WRITE(30,221)(COIAM(I),I=1,20),(L)
        221       FORMAT(1X,20(F4.2,','),15X,I4)
                  L=L+1
      	        LINE=LINE+1
                  WRITE(*,*) 'Processing Line ',LINE, 'Overwriting COIAM'
                  WRITE(12,*) 'Processing Line ',LINE, 'Overwriting COIAM'
        902     CONTINUE
                CLOSE(11)
      	    ENDIF
      !     END CHECK WHERE COIAM SHOULD BE OVERWRITTEN   =========================
      !     CHECK WHERE CAY SHOULD BE OVERWRITTEN   ===============================
              IF(LINE.EQ.LINECAY) THEN
                OPEN(UNIT=11,FILE=VARFILE)
                READ(11,111)DUM
      	      READ(11,111)DUM
                DO 903 WHILE (L.LE.ISUBNO)
                  READ(11,*)D1,D2, &
                    (COIAM(I),I=1,20),(CAY(I),I=1,20), &
                    (ELAIM(I),I=1,20),(ROOTA(I),I=1,20), &
                    (ICC(I),I=1,20),(ALBEDO(I),I=1,20)
                  READ(20,111)DUM
      	        WRITE(30,221)(CAY(I),I=1,20),(L)
                  L=L+1
      	        LINE=LINE+1
                  WRITE(*,*) 'Processing Line ',LINE, 'Overwriting CAY'
                  WRITE(12,*) 'Processing Line ',LINE, 'Overwriting CAY'
        903     CONTINUE
                CLOSE(11)
              ENDIF
      !     END CHECK WHERE CAY SHOULD BE OVERWRITTEN   ==========================
      !     CHECK WHERE ELAIM SHOULD BE OVERWRITTEN  =============================
              IF(LINE.EQ.LINEELAIM) THEN
                OPEN(UNIT=11,FILE=VARFILE)
                READ(11,111)DUM
      	      READ(11,111)DUM
                DO 904 WHILE (L.LE.ISUBNO)
                  READ(11,*)D1,D2, &
                     (COIAM(I),I=1,20),(CAY(I),I=1,20), &
                     (ELAIM(I),I=1,20),(ROOTA(I),I=1,20), &
                     (ICC(I),I=1,20),(ALBEDO(I),I=1,20)
                  READ(20,111)DUM
      	        WRITE(30,221)(ELAIM(I),I=1,20),(L)
                  L=L+1
      	        LINE=LINE+1
                  WRITE(*,*) 'Processing Line ',LINE, 'Overwriting ELAIM'
                  WRITE(12,*) 'Processing Line ',LINE, 'Overwriting ELAIM'
        904     CONTINUE
                CLOSE(11)
              ENDIF
      !     END CHECK WHERE ELAIM SHOULD BE OVERWRITTEN   =============================
      !     CHECK WHERE ROOTA SHOULD BE OVERWRITTEN  ==================================
              IF(LINE.EQ.LINEROOTA) THEN
                OPEN(UNIT=11,FILE=VARFILE)
                READ(11,111)DUM
      	      READ(11,111)DUM
                DO 905 WHILE (L.LE.ISUBNO)
                  READ(11,*)D1,D2, &
                     (COIAM(I),I=1,20),(CAY(I),I=1,20), &
                     (ELAIM(I),I=1,20),(ROOTA(I),I=1,20), &
                     (ICC(I),I=1,20),(ALBEDO(I),I=1,20)
                  READ(20,111)DUM
      	        WRITE(30,221)(ROOTA(I),I=1,20),(L)
                  L=L+1
      	        LINE=LINE+1
                  WRITE(*,*) 'Processing Line ',LINE, 'Overwriting ROOTA'
                  WRITE(12,*) 'Processing Line ',LINE, 'Overwriting ROOTA'
        905     CONTINUE
                CLOSE(11)
              ENDIF
      !    END CHECK WHERE ROOTA SHOULD BE OVERWRITTEN   =============================
      !    CHECK WHERE ICC SHOULD BE OVERWRITTEN  ==================================
              IF(LINE.EQ.LINEICC) THEN
                OPEN(UNIT=11,FILE=VARFILE)
                READ(11,111)DUM
      	      READ(11,111)DUM
                DO 906 WHILE (L.LE.ISUBNO)
                  READ(11,*)D1,D2, &
                     (COIAM(I),I=1,20),(CAY(I),I=1,20), &
                     (ELAIM(I),I=1,20),(ROOTA(I),I=1,20), &
                     (ICC(I),I=1,20),(ALBEDO(I),I=1,20)
                  READ(20,111)DUM
      	        WRITE(30,226)(ICC(I),I=1,20),(L)
        226       FORMAT(2X,20(I3,2X),6X,'*****',5X,I4)
                  L=L+1
      	        LINE=LINE+1
                  WRITE(*,*) 'Processing Line ',LINE, 'Overwriting ICC'
                  WRITE(12,*) 'Processing Line ',LINE, 'Overwriting ICC'
        906     CONTINUE
                CLOSE(11)
              ENDIF
      !     END CHECK WHERE ICC SHOULD BE OVERWRITTEN   ============================
      !     COPY MENU LINES *************************************************
              WRITE(30,111) DUM
      	    WRITE(*,*) 'PROCESSING LINE ',LINE
              WRITE(12,*) 'PROCESSING LINE ',LINE
              LINE=LINE+1
        900 CONTINUE
        999 CLOSE(20)
            CLOSE(30)
      !***********************************************************************
      ! Time Elapsed
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
            WRITE(*,*)
            WRITE(*,*) debugSTAT, ' Time Elapsed : ', REAL(COUNT_1 - COUNT_0)/ REAL(COUNT_RATE)
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
            WRITE(12,*)
            WRITE(12,*) debugSTAT, ' Time Elapsed : ', REAL(COUNT_1 - COUNT_0)/ REAL(COUNT_RATE)
            WRITE(12,*)
            WRITE(12,*) 'END OF PROGRAM. '
            CLOSE(12)
         	  STOP
      END PROGRAM ACRU_MENU_CALIBRATION
