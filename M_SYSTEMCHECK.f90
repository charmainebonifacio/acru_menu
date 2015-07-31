!###################################################################
! MODULE TITLE : M_SYSTEMCHECK
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 30, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will be used to check the date and time.
!###################################################################
module m_systemcheck

    implicit none

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  DATETIMELOG
!       DESCRIPTION  :  This subroutine will calculate the date and time.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Character, OUTPUT, date of the run (YYYY_MM_DD format)
!                       Character, OUTPUT, date of the run (YYYY-MM-DD format)
!                       Character, OUTPUT, time of the run
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
!       DESCRIPTION  :  This subroutine will calculate the elapsed time.
!       AUTHORED BY  :  Charmaine Bonifacio
!      DATE REVISED  :  July 30, 2015
!        PARAMETERS  :  Integer, OUTPUT, total time the program ran
!                       Integer, INPUT, start count
!                       Integer, INPUT, end count
!                       Integer, INPUT, rate count
!
!-------------------------------------------------------------------------------
   subroutine elapsedtime(elapsed_time, sys_count_0, sys_count_1, countrate)

       integer, intent(in) :: sys_count_0, sys_count_1, countrate
       real, intent(out) :: elapsed_time
       elapsed_time = 0
       elapsed_time = real(sys_count_1 - sys_count_0)/ real(countrate)

   end subroutine elapsedtime

end module m_systemcheck

!###################################################################
! MODULE TITLE : M_SYSTEMLOG
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 27
