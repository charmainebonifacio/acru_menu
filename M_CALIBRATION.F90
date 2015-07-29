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
