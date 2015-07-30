!###################################################################
! MODULE TITLE : M_SYSTEMLOG
!-------------------------------------------------------------------
! CREATED BY   : Charmaine Bonifacio
! DATE CREATED : July 27, 2015
! DATE REVISED : July 29, 2015
!-------------------------------------------------------------------
! DESCRIPTION  : The module will contain various subroutines
!                needed to format the LOG FILE.
!###################################################################
module m_systemlog

    implicit none
    character(11), parameter :: debugStat = '[ STATUS ] '
    character(11), parameter :: debugRes = '[ RESULT ] '
    character(11), parameter :: debugLog = '[ LOGGED ] '
    character(67), parameter :: programHeader = "###################################################################"
    character(20), parameter :: dayStat = '             DATE : '
    character(20), parameter :: timeStat = '             TIME : '
    character(20), parameter :: etimeStat = '     ELAPSED TIME : '
    character(20), parameter :: logfileStat = '          LOGFILE : '
    character(20), parameter :: fileNameOpened =  '  FILENAME OPENED : '
    character(20), parameter :: fileStat =  '      FILE STATUS : '
    character(81), parameter :: lineHeader = '================================================================================='
    character(len=*), parameter:: format_status_line = '( A11, A80 )'
    save

contains

    subroutine startprogramlog(unit_no)

        integer, intent(in) :: unit_no

        write(unit_no,*)
        write(unit_no,*) 'START OF PROGRAM. '
        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)
        write(unit_no,*) ' THE ACRU_MENU PROGRAM WILL COPY VALUES FROM A TAB-DELIMITED FILE. '
        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)

    end subroutine startprogramlog

    subroutine endprogramlog(unit_no)

        integer, intent(in) :: unit_no

        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)
        write(unit_no,*) '   THE ACRU_MENU PROGRAM HAS FINISHED UPDATING THE MENU FILE. '
        write(unit_no,*)
        write(unit_no,*) programHeader
        write(unit_no,*)
        write(unit_no,*) 'END OF PROGRAM. '
     end subroutine endprogramlog

   subroutine filestatcheck(status, unit_no)

       integer, intent(in) :: unit_no, status

       if (status==0) then
           write(unit_no,format_status_line) debugStat, ' SUCCESSFULLY OPENED FILE.'
       end if
       if (status/=0) then
           write(unit_no,format_status_line) debugStat, ' COULD NOT OPEN FILE.'
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

end module m_systemlog
