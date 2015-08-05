!###################################################################
! MODULE TITLE : M_SYSTEMLOG
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : JULY 27, 2015
! DATE REVISED : AUGUST 1, 2015
! DESCRIPTION  : THE MODULE WILL CONTAIN VARIOUS SUBROUTINES
!                NEEDED TO FORMAT THE LOG FILE.
!###################################################################
module m_systemlog

    implicit none
    character(11), parameter :: debugStat = '[ STATUS ] '
    character(11), parameter :: debugRes = '[ RESULT ] '
    character(11), parameter :: debugLog = '[ LOGGED ] '
    character(36), parameter :: logHeader = '**************************************'
    character(67), parameter :: programHeader = "###################################################################"
    character(20), parameter :: dayStat = '             DATE : '
    character(20), parameter :: timeStat = '             TIME : '
    character(20), parameter :: etimeStat = '     ELAPSED TIME : '
    character(20), parameter :: logfileStat = '          LOGFILE : '
    character(20), parameter :: fileNameOpened =  '  FILENAME OPENED : '
    character(20), parameter :: fileStat =  '      FILE STATUS : '
    character(81), parameter :: sectionHeader = '================================================================================='
    character(len=*), parameter:: format_status_line = '( 1X, A11, A26 )'
    save

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  HEADERLOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT OUT THE HEADER CONTAINING
!                       SPECIFIC INFORMATION.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 1, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
    subroutine headerlog(unit_no)

        integer, intent(in) :: unit_no

        write(unit_no,*)
        write(unit_no,*) logHeader
        write(unit_no,*)
        write(unit_no,*) '        ACRU MENU LOG FILE'
        write(unit_no,*)
        write(unit_no,*) ' CREATED BY   : Charmaine Bonifacio'
        write(unit_no,*) ' DATE REVISED : August 1, 2015'
        write(unit_no,*)
        write(unit_no,*) logHeader

    end subroutine headerlog

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  STARTPROGRAMLOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT OUT THE START LOG
!                       HEADER FOR THIS SCRIPT.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  ENDPROGRAMLOG
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT OUT THE END LOG
!                       HEADER FOR THIS SCRIPT.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
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

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  FILESTATCHECK
!       DESCRIPTION  :  THIS SUBROUTINE WILL CHECK IF THE FILE WAS OPENED
!                       SUCCESSFULLY.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, STATUS VALUE 0 IS SUCCESSFUL
!
!-------------------------------------------------------------------------------
   subroutine filestatcheck(status, unit_no)

       integer, intent(in) :: unit_no, status

       if (status==0) then
           write(unit_no,format_status_line) debugStat, ' SUCCESSFULLY OPENED FILE.'
       end if
       if (status/=0) then
           write(unit_no,format_status_line) debugStat, ' COULD NOT OPEN FILE.'
       end if

   end subroutine filestatcheck

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  VALUECHECK
!       DESCRIPTION  :  THIS SUBROUTINE WILL CHECK THE VALIDITY OF THE
!                       ISUBNO VALUE BEFORE PROCEEDING WITH THE CALIBRATION.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, INPUT, VALUE 0 MEANS NO ISUBNO FOUND
!                    :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                    :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!
!-------------------------------------------------------------------------------
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
