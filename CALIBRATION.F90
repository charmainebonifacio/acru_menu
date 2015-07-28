!###################################################################
! MODULE TITLE : CALIBRATION
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
MODULE CALIBRATION
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
   SUBROUTINE ELAPSEDTIME(ELAPSED_TIME,SYS_COUNT_0,SYS_COUNT_1,COUNTRATE)
      REAL, INTENT(OUT) :: ELAPSED_TIME
      INTEGER, INTENT(IN) :: SYS_COUNT_0,SYS_COUNT_1,COUNTRATE
      ELAPSED_TIME = 0
      ELAPSED_TIME = REAL(SYS_COUNT_1 - SYS_COUNT_0)/ REAL(COUNTRATE)
   END SUBROUTINE ELAPSEDTIME
   SUBROUTINE CALCVARLINE(LINE_VAR, ISUBNO, VAR_ROW)
      INTEGER, INTENT(OUT) :: LINE_VAR
      INTEGER, INTENT(IN) :: ISUBNO, VAR_ROW
      LINE_VAR = 0
      LINE_VAR = 23 + (ISUBNO + 5) * VAR_ROW
   END SUBROUTINE CALCVARLINE
END MODULE CALIBRATION
