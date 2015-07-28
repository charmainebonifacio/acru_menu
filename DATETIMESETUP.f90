!###################################################################
! MODULE TITLE : M_DATETIMESETUP
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
MODULE M_DATETIMESETUP
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
   SUBROUTINE FILESTATCHECK(STATUS,UNIT_NO)
      INTEGER, INTENT(IN) :: UNIT_NO, STATUS
      IF (STATUS==0) THEN
         WRITE(UNIT_NO,*) 'SUCCESSFULLY OPENED FILE.'
      END IF
      IF (STATUS/=0) THEN
         WRITE(UNIT_NO,*) 'COULD NOT OPEN FILE.'
      END IF
   END SUBROUTINE FILESTATCHECK
   SUBROUTINE VALUECHECK(STATUS,VALUE,UNIT_NO)
      INTEGER, INTENT(IN) :: UNIT_NO, VALUE
      LOGICAL, INTENT(OUT) :: STATUS
      IF (STATUS==0) THEN
         WRITE(UNIT_NO,*) ' INVALID VALUE. '
         STATUS = 0 ! FALSE
      END IF
   END SUBROUTINE VALUECHECK
   SUBROUTINE ELAPSEDTIME(ELAPSED_TIME, SYS_COUNT_0, SYS_COUNT_1, COUNTRATE)
      REAL, INTENT(OUT) :: ELAPSED_TIME
      INTEGER, INTENT(IN) :: SYS_COUNT_0, SYS_COUNT_1, COUNTRATE
      ELAPSED_TIME = 0
      ELAPSED_TIME = REAL(SYS_COUNT_1 - SYS_COUNT_0)/ REAL(COUNTRATE)
   END SUBROUTINE ELAPSEDTIME
END MODULE M_DATETIMESETUP
