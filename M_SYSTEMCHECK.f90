!###################################################################
! MODULE TITLE : M_SYSTEMCHECK
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 24, 2015
! DATE REVISED : July 28, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain DATE and TIME subroutines.
! SUBROUTINE 1 : This subroutine will calculate the date and time.
! SUBROUTINE 2 : This subroutine will calculate the elapsed time.
! SUBROUTINE 3 : This subroutine will check if the file was opened
!                successfully.
! SUBROUTINE 4 : This subroutine will check ISUBNO value.
!###################################################################
module m_systemcheck

    use m_systemlog, only: programHeader
    implicit none

contains

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

   subroutine elapsedtime(elapsed_time, sys_count_0, sys_count_1, countrate)

       integer, intent(in) :: sys_count_0, sys_count_1, countrate
       real, intent(out) :: elapsed_time
       elapsed_time = 0
       elapsed_time = real(sys_count_1 - sys_count_0)/ real(countrate)

   end subroutine elapsedtime

   subroutine filestatcheck(status, unit_no)

       integer, intent(in) :: unit_no, status

       if (status==0) then
           write(unit_no,*) 'SUCCESSFULLY OPENED FILE.'
       end if
       if (status/=0) then
           write(unit_no,*) 'COULD NOT OPEN FILE.'
       end if

   end subroutine filestatcheck

    subroutine valuecheck(value, unit_no1, unit_no2)

        integer, intent(in) :: unit_no1, unit_no2, value

        if (value==0) then
            write(unit_no1,*)
            write(unit_no1,*) programHeader
            write(unit_no1,*)
            write(unit_no1,*) 'INVALID VALUE. TERMINATING PROGRAM'
            write(unit_no1,*)
            close(unit_no1)
            close(unit_no2)
            stop ' INVALID VALUE. TERMINATING PROGRAM. '
        end if

    end subroutine valuecheck

end module m_systemcheck
