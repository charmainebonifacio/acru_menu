!###############################################################################
! MODULE TITLE : M_SYSTEMCHECK
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : JULY 24, 2015
! DATE REVISED : JULY 30, 2015
! DESCRIPTION  : THE MODULE CONTAINS SUBROUTINES TO CHECK THE DATE AND TIME.
!###############################################################################
module m_systemcheck

    implicit none

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  DATETIMELOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE DATE AND TIME.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  CHARACTER, OUTPUT, DATE OF THE RUN (YYYY_MM_DD FORMAT)
!                       CHARACTER, OUTPUT, DATE OF THE RUN (YYYY-MM-DD FORMAT)
!                       CHARACTER, OUTPUT, TIME OF THE RUN
!
!-------------------------------------------------------------------------------
   subroutine datetimelog(date, datenow, timenow)

       character(len=8) :: dateinfo
       character(len=4) :: year, month*2, day*2
       character(len=2) :: hrs, min, sec*6
       character(len=10) :: timeinfo
       character(len=10), intent(out) :: date, datenow
       character(len=12), intent(out) :: timenow
       call date_and_time(dateinfo, timeinfo)
       year = dateinfo(1:4)
       month = dateinfo(5:6)
       day = dateinfo(7:8)
       date = year // '_' // month // '_' // day
       datenow = year // '-' // month // '-' // day
       hrs = timeinfo(1:2)
       min = timeinfo(3:4)
       sec = timeinfo(5:10)
       timenow = hrs // ':' // min // ':' // sec

   end subroutine datetimelog

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  ELAPSEDTIME
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE ELAPSED TIME.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, OUTPUT, TOTAL TIME THE PROGRAM RAN
!                       INTEGER, INPUT, START COUNT
!                       INTEGER, INPUT, END COUNT
!                       INTEGER, INPUT, RATE COUNT
!
!-------------------------------------------------------------------------------
   subroutine elapsedtime(elapsed_time, sys_count_0, sys_count_1, countrate)

       integer, intent(in) :: sys_count_0, sys_count_1, countrate
       real, intent(out) :: elapsed_time
       elapsed_time = 0
       elapsed_time = real(sys_count_1 - sys_count_0)/ real(countrate)

   end subroutine elapsedtime

end module m_systemcheck
