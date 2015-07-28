!###################################################################
! MODULE TITLE : CALIBRATION
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 26, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed for the program to work.
! SUBROUTINE 1 : This subroutine will calculate the row line
!                associated with the variable.
!###################################################################
MODULE CALIBRATION
IMPLICIT NONE
CONTAINS
   SUBROUTINE CALCVARLINE(LINEVAR, totalHRU, numRow)
      INTEGER, INTENT(OUT) :: LINEVAR
      INTEGER, INTENT(IN) :: totalHRU, numRow
      LINEVAR = 0
      LINEVAR = 23 + (totalHRU + 5) * numRow
   END SUBROUTINE CALCVARLINE
END MODULE CALIBRATION
