!###################################################################
! MODULE TITLE : M_SYSTEMLOG
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 27, 2015
! DATE REVISED : July 28, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed for the LOG FILE to work.
! SUBROUTINE 1 : This subroutine will print out the start log
!                header for this script.
! SUBROUTINE 2 : This subroutine will print out the end log
!                header for this script.
!###################################################################
MODULE M_SYSTEMLOG
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
END MODULE M_SYSTEMLOG
