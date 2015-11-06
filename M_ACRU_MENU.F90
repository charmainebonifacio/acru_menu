!###############################################################################
! MODULE TITLE : M_ACRU_MENU
! CREATED BY   : CHARMAINE BONIFACIO
! DATE CREATED : JULY 24, 2015
! DATE REVISED : SEPTEMBER 7, 2015
! DESCRIPTION  : THE MODULE CONTAINS SUBROUTINES NEEDED TO PROCESS THE MENU FILE.
!###############################################################################
module m_acru_menu

    use m_systemlog, only: debugStat, debugRes, sectionHeader
    implicit none

    character(len=*), parameter:: format_line = '( A80 )'
    character(len=*), parameter:: format_read_line = '( A1 )'
    character(len=*), parameter:: format_var_header = '( 1X,A11,A50,I7 )'
    character(len=*), parameter:: format_block_string = '( 16X,A50 )'
    character(len=*), parameter:: format_print_var = '( 1X,A11,A55,I7 )'
    character(len=*), parameter:: format_var_summary = '( 1X,A11,A20,I7 )'
    character(len=*), parameter:: format_isubno = '( 3X,I4 )'
    character(len=*), parameter:: format_location_line = '( 23X,F5.2,5X,I1)'
    character(len=*), parameter:: format_location = '( F8.2,1X,F6.3,1X,F6.1,1X,F5.2,5X,I1,1X,F8.1,33X,I4 )'
    character(len=*), parameter:: format_lr = '( 12(F6.2),4X,I4 )'
    character(len=*), parameter:: format_soils = '( 1X,F5.2,2X,F5.2,6(1X,F4.3),2(2X,F5.2),19X,I4 )'
    character(len=*), parameter:: format_albedo_line = '( 66X,I1,5X,I1 )'
    character(len=*), parameter:: format_albedo = '( 1X,12(F4.2,1X),5X,I1,5X,I1,3X,I4 )'
    character(len=*), parameter:: format_cerc = '( 1X,12(F4.2,1X),15X,I4 )'
    character(len=*), parameter:: format_strmflw_line = '( 25X,I1,2(2X,F5.3),3X,F4.2,29X,I4 )'
    character(len=*), parameter:: format_strmflw = '( 1X,F5.2,3X,F5.3,1X,F6.2,4X,I1,2(2X,F5.3),3X,F4.2,29X,I4 )'
    character(len=*), parameter:: format_icc = '( 2X,12(I3.2,2X),14X,I4 )'
    character(len=*), parameter:: format_snow_line = '( 4X,I1,21X,2(4X,I1),3X,F4.2,2(2X,F4.2),5X,I1,1X,F5.2 )'
    character(len=*), parameter:: format_snow = '( 4X,I1,3(6X,I1),2(4X,I1),3X,F4.2,2(2X,F4.2),5X,I1,1X,F5.2,9X,I4 )'
    character(len=*), parameter:: format_adjustment = '( 1X,A11,A30,I7,A9,I4 )'
    character(len=*), parameter:: format_icelln_line = '( 19X,I1 )'
    character(len=*), parameter:: format_icelln = '( 2X,I4,3X,I4,6X,I1,56X,I4 )'
    character(len=*), parameter:: format_irainf = '( A41,35X,I4 )'
    character(len=*), parameter:: format_rain_line = '( 16X,I5,5X,I1 )'
    character(len=*), parameter:: format_rain = '( 6X,I1,6X,I1,2X,I5,5X,I1,49X,I4 )'
    character(len=*), parameter:: format_corppt = '( 12(1X,F4.2),16X,I4 )'
    character(len=*), parameter:: format_iobstq_line = '( 11X,I1,6X,I1)'
    character(len=*), parameter:: format_iobstq = '( 5X,I1,2(6X,I1),56X,I4 )'
    character(len=*), parameter:: format_corfac = '( 12(1X,F5.3),4X,I4 )'
    character(len=*), parameter:: format_slorad = '( 12(1X,F5.2),4X,I4 )'
    character(len=*), parameter:: format_lcover_line = '( 12X,I1,2X,F5.2,1X,F6.1,1X,F8.2 )'
    character(len=*), parameter:: format_lcover = '( 5X,I1,6X,I1,2X,F5.2,1X,F6.1,1X,F8.2,40X,I4 )'
    character(len=*), parameter:: format_telev = '( F6.1,4X,I2,64X,I4 )'
    character(len=*), parameter:: format_telev_line = '( 10X,I2,64X,I4 )'

contains

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  CALCVARLINE
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE ROW LINE
!                       ASSOCIATED WITH THE VARIABLE.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  JULY 30, 2015
!        PARAMETERS  :  INTEGER, OUTPUT THE ROW LINE FOR EACH VARIABLE
!                       INTEGER, INPUT, TOTAL NUMBER OF CATCHMENT IN WATERSHED
!                       INTEGER, INPUT THE VARIABLE RANK WITHIN MENU FILE.
!
!-------------------------------------------------------------------------------
    subroutine calcvarline(line_var, isubno, var_rank)

        integer, intent(out) :: line_var
        integer, intent(in) :: isubno, var_rank

        line_var = 0
        line_var = 23 + (isubno + 5) * var_rank

    end subroutine calcvarline

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  FINDTOTALCATCHMENTNUMBER
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE ISUBNO VARIABLE WITH
!                       TOTAL NUMBER OF HRU IN THE MENU FILE
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 24, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH OPENED FILE
!                       INTEGER, INPUT, TOTAL NUMBER OF CATCHMENT IN WATERSHED
!
!-------------------------------------------------------------------------------
    subroutine findTotalCatchmentNumber(unit_menu, isub_no)

        character(5) :: menuheaderline
        integer :: p, num_line
        integer, intent(in) :: unit_menu
        integer, intent(out) :: isub_no

        num_line = 11
        p = 1
        do 898 while (p < num_line)
            read(unit_menu,format_line) menuheaderline
            p = p + 1
        898 end do
        read(unit_menu,format_isubno) isub_no

    end subroutine findTotalCatchmentNumber

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  CALCULATEEOF
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE EOF FOR MENU FILE
!                       BASED ON THE TOTAL NUMBER OF CATCHMENTS IN THE WATERSHED
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 5, 2015
!        PARAMETERS  :  INTEGER, INPUT, TOTAL NUMBER OF HRU IN MENU
!                       INTEGER, OUTPUT, END OF FILE VALUE USING HRU CALCULATION
!
!-------------------------------------------------------------------------------
    subroutine calculateEOF(isub_no, line_eof)

        integer, intent(in) :: isub_no
        integer, intent(out) :: line_eof

        line_eof = 0
        line_eof = 23 + ((isub_no + 5) * 145) + isub_no

    end subroutine calculateEOF

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  CALCULATETOTLINES
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALCULATE THE TOTAL NUMBER OF LINES
!                       BASED ON READING THE FILE
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 24, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH OPENED FILE
!                       INTEGER, INPUT, THE TOTAL NUMBER OF LINES PROCESSED
!                       INTEGER, OUTPUT, THE NUMBER OF LINE READ
!
!-------------------------------------------------------------------------------
    subroutine calculateTOTLINES(unit_menu, tot_lines)

        character(5) :: dum
        integer :: i
        integer, intent(in) :: unit_menu
        integer, intent(inout) :: tot_lines

        i = 0
        do
            read(unit_menu,format_read_line,END=1) dum ! Simply read the space
            i = i + 1
        end do
     1  continue
        tot_lines = i

    end subroutine calculateTOTLINES

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  VALIDATEEOF
!       DESCRIPTION  :  THIS SUBROUTINE WILL VALIDATE THE ACTUAL EOF VALUE
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 21, 2015
!        PARAMETERS  :  INTEGER, INPUT, THE TOTAL NUMBER OF LINES PROCESSED
!                       INTEGER, INPUT, THE NUMBER OF LINE READ
!                       INTEGER, OUTPUT, STATUS OF VALIDATION
!
!-------------------------------------------------------------------------------
    subroutine validateEOF(line_eof, tot_lines, validation)

        integer, intent(in) :: line_eof, tot_lines
        integer, intent(out) :: validation

        if (tot_lines == line_eof) then
            validation = 0
        end if
        if (tot_lines < line_eof) then ! LESS THAN EOF
            validation = 1
        end if
        if (tot_lines > line_eof) then ! MORE THAN EOF
            validation = 2
        end if

    end subroutine validateEOF

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  INITIATIATEVARLINE
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE STARTING LINES FOR
!                       ALL ASSOCIATED VARIABLES
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 6, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, TOTAL NUMBER OF VARIABLES
!                       INTEGER, INPUT, TOTAL NUMBER OF CATCHMENT IN WATERSHED
!                       INTEGER ARRAY, INPUT, BLOCK NUMBER OF CONTAINER
!                       INTEGER ARRAY, OUTPUT, STARTING LINE FOR EACH BLOCK
!
!-------------------------------------------------------------------------------
    subroutine initiatiateVarLine(num_var, isub_no, block_container, block_line)

        integer :: i
        integer, intent(in) :: num_var, isub_no
        integer, dimension(num_var), intent(in) :: block_container
        integer, dimension(num_var), intent(out) :: block_line

        do i=1, num_var ! Initiate block container with specific numbers
            call calcvarline(block_line(i), isub_no, block_container(i))
        end do

    end subroutine initiatiateVarLine

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  PRINTRESULTS
!       DESCRIPTION  :  THIS SUBROUTINE WILL PRINT THE RESULTS OF ARRAY
!                       INITIALIZATION
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 5, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH OPENED FILE
!                       INTEGER, INPUT, TOTAL NUMBER OF VARIABLES
!                       CHARACTER ARRAY, INPUT, ALL TYPES OF VARIABLES
!                       INTEGER ARRAY, INPUT, BLOCK NUMBER OF CONTAINER
!                       INTEGER ARRAY, INPUT, STARTING LINE FOR EACH BLOCK
!
!-------------------------------------------------------------------------------
    subroutine printResults(unit_no, num_var, block_var, block_container, block_line)

        integer :: i
        integer, intent(in) :: unit_no, num_var
        character(len=50), dimension(num_var), intent(in) :: block_var
        integer, dimension(num_var), intent(in) :: block_container, block_line

        write(unit_no,*) ' >> INITIALIZING THE FOLLOWING BLOCK FOR EACH VARIABLES... '
        write(unit_no,*)
        do i=1, num_var
          write(unit_no,format_print_var) debugStat, block_var(i)//': ', block_container(i)
        end do
        write(unit_no,*)
        write(unit_no,*) ' >> INITIALIZING THE FOLLOWING STARTING LINES FOR EACH VARIABLES... '
        write(unit_no,*)
        do i=1, num_var
          write(unit_no,format_print_var) debugStat, block_var(i)//': ', block_line(i)
        end do

    end subroutine printResults

!-------------------------------------------------------------------------------
!
!       P A R A M E T E R   C A L I B R A T I O N   S U B R O U T I N E S
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  INITIATEVARCALIBRATIONBLOCK
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE ARRAY WITH DIFFERENT
!                       TYPES OF VARIABLES
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 21, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH OPENED FILE
!                       INTEGER, INPUT, TOTAL NUMBER OF VARIABLES
!                       CHARACTER ARRAY, INPUT, BLOCK INFO CONTAINING VARIABLES
!
!-------------------------------------------------------------------------------
    subroutine initiateVarCalibrationBlock(num_var, block_var)

        integer, intent(in) :: num_var
        character(len=50), dimension(num_var), intent(out) :: block_var

        block_var(1) = ' REFERENCE POTENTIAL EVAPORATION UNIT ---- TMXLR ' ! REFERENCE POTENTIAL EVAPORATION UNIT INFO
        block_var(2) = ' REFERENCE POTENTIAL EVAPORATION UNIT ---- TMNLR ' ! REFERENCE POTENTIAL EVAPORATION UNIT INFO
        block_var(3) = ' SOILS ------- DEPAB,WP1/2,FC1/2,PO1/2,AB/BFRESP ' ! CATCHMENT SOILS INFO: DEPAHO, DEPBHO, WP1, WP2, FC1, FC2, PO1, PO2, ABRESP, BFRESP
        block_var(4) = ' STREAMFLOW SIM CONTROL ---- QFRESP,COFRU,SMDDEP ' ! STREAMFLOW SIMULATION CONTROL VARIABLES: QFRESP, COFRU, SMDDEP, IRUN, ADJIMP, DISIMP, STOIMP
        block_var(5) = ' SNOW VARIABLES: -------- ISNOTP, IPSCOR, ISCREE ' ! SNOW MAIN VARIABLE: ISNOTP, IPSCOR, ISCREE

    end subroutine initiateVarCalibrationBlock

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  INITIATEVARCALIBRATIONCONTAINER
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE ARRAY WITH BLOCK
!                       NUMBER FOR EACH VARIABLE
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 21, 2015
!        PARAMETERS  :  INTEGER, INPUT, TOTAL NUMBER OF VARIABLES
!                       INTEGER ARRAY, OUTPUT, BLOCK NUMBER OF CONTAINER
!
!-------------------------------------------------------------------------------
    subroutine initiateVarCalibrationContainer(num_var, block_container)

        integer, intent(in) :: num_var
        integer, dimension(num_var), intent(out) :: block_container

        block_container(1) = 20
        block_container(2) = 21
        block_container(3) = 43
        block_container(4) = 66
        block_container(5) = 134

    end subroutine initiateVarCalibrationContainer

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  CALIBRATELINE
!       DESCRIPTION  :  THIS SUBROUTINE WILL CALIBRATE THE VARIABLES
!                       ACCORDING TO THE LINE NUMBER.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  AUGUST 21, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, TOTAL NUMBER OF CATCHMENT IN WATERSHED
!                       INTEGER, OUTPUT THE TOTAL NUMBER OF LINES PROCESSED
!                       INTEGER, INPUT, THE INDEX ASSOCIATED WITH A VARIABLE
!
!-------------------------------------------------------------------------------
    subroutine calibrateline(unit_no, unit_oldMenu, unit_menu, unit_var, isubno, &
                             line, var_index, block_var_string)

        character(len=50), intent(in) :: block_var_string
        integer, intent(in) :: isubno, unit_no, unit_oldMenu, unit_menu, unit_var, var_index
        integer, intent(inout) :: line
        character(80) :: dum, dum2
        integer ::  i, l, d1, d2
        integer :: irun, isnow, isnotp, ipscor, iscree, iexp, ifor, mcdmod
        real :: d3
        real :: depaho, depbho, wp1, wp2, fc1, fc2, po1, po2, abresp, bfresp
        real :: qfresp, cofru, smddep, adjimp, disimp, stoimp
        real :: depaho2, depbho2, abresp2, bfresp2
        real :: qfresp2, qfresp3, cofru2, smddep2, smddep3
        real :: sncapi, snorc, snirc, sncc
        real, dimension(12) :: tmaxlr, tminlr

        l = 1
        read(unit_var,*) dum2  ! calibration header
        read(unit_var,*) dum2  ! monthly header
        read(unit_var,*) dum2  ! variable header
        write(unit_no,*) sectionHeader
        write(unit_no,format_block_string) block_var_string ! variable to be processed
        write(unit_no,format_var_header) debugStat,' PARAMETER ADJUSTMENT STARTING FROM LINE '//' : ', line
        do 700 while (l <= isubno)
            read(unit_var,*) d1, d2, d3, &
                             (tmaxlr(i),i=1,12), (tminlr(i),i=1,12), &
                             depaho, depbho, depaho2, depbho2, wp1, wp2, fc1, fc2, &
                             po1, po2, abresp, bfresp, abresp2, bfresp2, &
                             qfresp, qfresp2, qfresp3, cofru, cofru2, &
                             smddep, smddep2, smddep3, isnotp, ipscor, iscree
            select case (var_index)
               case (1)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_lr) (tmaxlr(i),i=1,12),(l)
                   write(unit_menu,format_lr) (tmaxlr(i),i=1,12),(l)
               case (2)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_lr) (tminlr(i),i=1,12),(l)
                   write(unit_menu,format_lr) (tminlr(i),i=1,12),(l)
               case (3)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_soils) depaho2, depbho2, wp1, wp2, fc1, &
                                               fc2, po1, po2, abresp2, bfresp2, l
                   write(unit_menu,format_soils) depaho2, depbho2, wp1, wp2, fc1, &
                                               fc2, po1, po2, abresp2, bfresp2, l
               case (4)
                   read(unit_oldMenu,format_strmflw_line) irun, adjimp, disimp, stoimp
                   write(unit_no,format_strmflw) qfresp3, cofru2, smddep3, irun, &
                                                 adjimp, disimp, stoimp, l
                   write(unit_menu,format_strmflw) qfresp3, cofru2, smddep3, irun, &
                                                   adjimp, disimp, stoimp, l
               case (5)
                   read(unit_oldMenu,format_snow_line) isnow, iexp, ifor, sncapi, snorc, &
                                                  snirc, mcdmod, sncc
                   write(unit_no,format_snow) isnow, isnotp, ipscor, iscree, iexp, ifor, &
                                              sncapi, snorc, snirc, mcdmod, sncc, l
                   write(unit_menu,format_snow) isnow, isnotp, ipscor, iscree, iexp, ifor, &
                                              sncapi, snorc, snirc, mcdmod, sncc, l
           end select
           write(unit_no,format_adjustment) debugRes,' SUCCESSFULLY PROCESSED LINE ', line, ' & HRU # ',l
           l = l + 1
           line = line + 1
    700 end do

    end subroutine calibrateline

!-------------------------------------------------------------------------------
!
!          M E N U   I N I T I A L I Z A T I O N   S U B R O U T I N E S
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  INITIATEVARCALIBRATIONBLOCK
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE ARRAY WITH DIFFERENT
!                       TYPES OF VARIABLES
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  SEPTEMBER 7, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH OPENED FILE
!                       INTEGER, INPUT, TOTAL NUMBER OF VARIABLES
!                       CHARACTER ARRAY, INPUT, BLOCK INFO CONTAINING VARIABLES
!
!-------------------------------------------------------------------------------
    subroutine initiateVarInitializationBlock(num_var, block_var)

        integer, intent(in) :: num_var
        character(len=50), dimension(num_var), intent(out) :: block_var

        block_var(1) = ' SUBCATCHMENT CONFIG ------------ ICELLN, IDSTRM ' ! SUBCATCHMENT CONFIG: ICELLN, IDSTRM, PRTOUT
        block_var(2) = ' RAINFALL FILE ORG ---------------------- IRAINF ' ! RAINFALL FILE ORGANIZATION: IRAINF
        block_var(3) = ' RAINFALL INFO ------------------ FORMAT, PPTCOR ' ! RAINFALL INFO: FORMAT, PPTCOR, MAP, ARF
        block_var(4) = ' MONTHLY RAINFALL ADJUSTMENT FACTOR ----- CORPPT ' ! MONTHLY RAINFALL ADJUSTMENT FACTOR: CORPPT
        block_var(5) = ' AVAILABILITY OF STREAMFLOW DATA -------- IOBSTQ ' ! AVAILABILITY OF STREAMFLOW DATA: IOBSTQ, IOBSPK, IOBOVR
        block_var(6) = ' LOCATIONAL -------- CLAREA, SAUEF, ELEV, WSSIZE ' ! LOCATIONAL AND CATCHMENT INFO: CLAREA, SAUEF, ELEV, ALAT, IHEMI, WSSIZE
        block_var(7) = ' TEMPERATURE ADJUSTMENT ALTITUDE---------- TELEV ' ! TEMPERATURE ADJUSTMENT ALTITUDE: TELEV
        block_var(8) = ' MONTHLY WIND CORRECTION FACTOR --------- WINCOR ' ! MONTHLY WIND CORRECTION FACTOR: WINCOR
        block_var(9) = ' MONTHLY REL HUM CORRECTION FACTOR ------ RHUCOR ' ! MONTHLY RELATIVE HUMIDITY CORRECTION FACTOR: RHUCOR
        block_var(10) = ' REFERENCE POTENTIAL EVAPORATION UNIT --- ALBEDO ' ! REFERENCE POTENTIAL EVAPORATION UNIT INFO
        block_var(11) = ' MONTHLY SUNSHINE CORRECTION FACTOR ----- SUNCOR ' ! MONTHLY SUNSHINE CORRECTION FACTOR: SUNCOR
        block_var(12) = ' LEVEL OF LAND COVER -------------------- LCOVER ' ! LEVEL OF LAND COVER INFO: LCOVER
        block_var(13) = ' CATCHMENT LAND COVER ---------------------- CAY ' ! CATCHMENT LAND COVER INFO
        block_var(14) = ' CATCHMENT LAND COVER -------------------- ELAIM ' ! CATCHMENT LAND COVER INFO
        block_var(15) = ' CATCHMENT LAND COVER -------------------- ROOTA ' ! CATCHMENT LAND COVER INFO
        block_var(16) = ' STREAMFLOW SIM CONTROL ------------------ COIAM ' ! STREAMFLOW SIMULATION CONTROL VARIABLE: COIAM
        block_var(17) = ' MONTHLY RADIATION CORRECTION FACTOR ---- SLORAD ' ! MONTHLY RADIATION CORRECTION FACTOR FOR SLOPED SURFACES: SLORAD
        block_var(18) = ' MONTHLY RADIATION CORRECTION FACTOR ---- RADCOR ' ! MONTHLY RADIATION CORRECTION FACTOR RELATIVE TO BASE STATION: RADCOR
        block_var(19) = ' SNOW OPTION ------------------------------- ICC ' ! SNOW VARIABLE: ICC

    end subroutine initiateVarInitializationBlock

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  INITIATEVARINITIALIZATIONCONTAINER
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE ARRAY WITH BLOCK
!                       NUMBER FOR EACH VARIABLE
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  SEPTEMBER 7, 2015
!        PARAMETERS  :  INTEGER, INPUT, TOTAL NUMBER OF VARIABLES
!                       INTEGER ARRAY, OUTPUT, BLOCK NUMBER OF CONTAINER
!
!-------------------------------------------------------------------------------
    subroutine initiateVarInitializationContainer(num_var, block_container)

        integer, intent(in) :: num_var
        integer, dimension(num_var), intent(out) :: block_container

        block_container(1) = 0
        block_container(2) = 1
        block_container(3) = 2
        block_container(4) = 3
        block_container(5) = 4
        block_container(6) = 9
        block_container(7) = 19
        block_container(8) = 25
        block_container(9) = 27
        block_container(10) = 28
        block_container(11) = 30
        block_container(12) = 49
        block_container(13) = 53
        block_container(14) = 54
        block_container(15) = 56
        block_container(16) = 67
        block_container(17) = 139
        block_container(18) = 140
        block_container(19) = 141

    end subroutine initiateVarInitializationContainer

!-------------------------------------------------------------------------------
!
!  SUBROUTINE TITLE  :  INITIALIZELINE
!       DESCRIPTION  :  THIS SUBROUTINE WILL INITIATE THE VARIABLES
!                       ACCORDING TO THE BLOCK LINE NUMBER.
!       AUTHORED BY  :  CHARMAINE BONIFACIO
!      DATE REVISED  :  SEPTEMBER 7, 2015
!        PARAMETERS  :  INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, UNIT NUMBER ASSOCIATED WITH FILE OPENED
!                       INTEGER, INPUT, TOTAL NUMBER OF CATCHMENT IN WATERSHED
!                       INTEGER, OUTPUT THE TOTAL NUMBER OF LINES PROCESSED
!                       INTEGER, INPUT, THE INDEX ASSOCIATED WITH A VARIABLE
!
!-------------------------------------------------------------------------------
    subroutine initializeline(unit_no, unit_oldMenu, unit_menu, unit_var, isubno, &
                             line, var_block_index, block_var_string)

        character(50), intent(in) :: block_var_string
        integer, intent(in) :: isubno, unit_no, unit_oldMenu, unit_menu, unit_var, var_block_index
        integer, intent(inout) :: line
        character(80) :: dum, dum2
        character(41) :: irainf
        integer ::  i, l, d1, d2
        integer :: icelln, idstrm
        integer :: rainMap, arf, lrreg
        integer :: iobspk, iobovr, glacie
        integer :: icons, iswave, ihemi
        real :: clarea, sauef, elev, alat, wssize
        real :: glmulti, gdepth, garea, telev
        integer, dimension(12) :: icc
        real, dimension(12) :: albedo, cay, elaim, roota, coiam
        real, dimension(12) :: corppt, wincor, rhucor, suncor, slorad, radcor
        integer, parameter :: prtout = 0
        integer, parameter :: rainFormat = 1
        integer, parameter :: pptcor = 1
        integer, parameter :: iobstq = 1
        integer, parameter :: lcover = 1

        l = 1
        read(unit_var,*) dum2  ! monthly header
        read(unit_var,*) dum2  ! variable header
        write(unit_no,*) sectionHeader
        write(unit_no,format_block_string) block_var_string ! variable to be processed
        write(unit_no,format_var_header) debugStat,' INITIALIZED MENU STARTING FROM LINE '//' : ', line
        do 701 while (l <= isubno)
            read(unit_var,*) d1, d2, &
				icelln, idstrm, irainf, (corppt(i),i=1,12), clarea, sauef, elev, wssize, &
            	telev, (wincor(i),i=1,12), (rhucor(i),i=1,12), (albedo(i),i=1,12), &
            	(suncor(i),i=1,12), (cay(i),i=1,12), (elaim(i),i=1,12), (roota(i),i=1,12), &
            	(coiam(i),i=1,12), (slorad(i),i=1,12), (radcor(i),i=1,12), (icc(i),i=1,12)
            select case (var_block_index)
               case (1)
                   read(unit_oldMenu,format_line) dum ! read PRTOUT variable
                   write(unit_no,format_icelln) icelln, idstrm, prtout, l
                   write(unit_menu,format_icelln) icelln, idstrm, prtout, l
               case (2)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_irainf) irainf, l
                   write(unit_menu,format_irainf) irainf, l
			   case (3)
                   read(unit_oldMenu,format_rain_line) rainMap, arf  ! read MAP, ARF variables
                   write(unit_no,format_rain) rainFormat, pptcor, rainMap, arf, l
                   write(unit_menu,format_rain) rainFormat, pptcor, rainMap, arf, l
               case (4)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_corppt) (corppt(i),i=1,12),(l)
                   write(unit_menu,format_corppt) (corppt(i),i=1,12),(l)
               case (5)
                   read(unit_oldMenu,format_iobstq_line) iobspk, iobovr  ! read IOBSPK, IOBOVR variables
                   write(unit_no,format_iobstq) iobstq, iobspk, iobovr, l
                   write(unit_menu,format_iobstq) iobstq, iobspk, iobovr, l
               case (6)
                   read(unit_oldMenu,format_location_line) alat, ihemi ! read ALAT, IHEMI variables
                   write(unit_no,format_location) clarea, sauef, elev, &
                                               alat, ihemi, wssize, l
                   write(unit_menu,format_location) clarea, sauef, elev, &
                                                alat, ihemi, wssize, l
               case (7)
                   read(unit_oldMenu,format_telev_line) lrreg
                   write(unit_no,format_telev) telev, lrreg, l
                   write(unit_menu,format_telev) telev, lrreg, l
               case (8)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_corfac) (wincor(i),i=1,12),(l)
                   write(unit_menu,format_corfac) (wincor(i),i=1,12),(l)
               case (9)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_corfac) (rhucor(i),i=1,12),(l)
                   write(unit_menu,format_corfac) (rhucor(i),i=1,12),(l)
               case (10)
                   read(unit_oldMenu,format_albedo_line) icons,iswave ! read ICONS, ISWAVE variables
                   write(unit_no,format_albedo) (albedo(i),i=1,12), icons, iswave, l
                   write(unit_menu,format_albedo) (albedo(i),i=1,12), icons, iswave, l
               case (11)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_corfac) (suncor(i),i=1,12),(l)
                   write(unit_menu,format_corfac) (suncor(i),i=1,12),(l)
               case (12)
                   read(unit_oldMenu,format_lcover_line) glacie, glmulti, gdepth, garea ! read GLACIE, GLMULTI, GDEPTH, GAREA variables
                   write(unit_no,format_lcover) lcover, glacie, glmulti, gdepth, garea, l
                   write(unit_menu,format_lcover) lcover, glacie, glmulti, gdepth, garea, l
               case (13)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc) (cay(i),i=1,12),(l)
                   write(unit_menu,format_cerc) (cay(i),i=1,12),(l)
               case (14)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc) (elaim(i),i=1,12),(l)
                   write(unit_menu,format_cerc) (elaim(i),i=1,12),(l)
               case (15)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc) (roota(i),i=1,12),(l)
                   write(unit_menu,format_cerc) (roota(i),i=1,12),(l)
               case (16)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_cerc) (coiam(i),i=1,12),(l)
                   write(unit_menu,format_cerc) (coiam(i),i=1,12),(l)
               case (17)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_slorad) (slorad(i),i=1,12),(l)
                   write(unit_menu,format_slorad) (slorad(i),i=1,12),(l)
               case (18)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_corfac) (radcor(i),i=1,12),(l)
                   write(unit_menu,format_corfac) (rhucor(i),i=1,12),(l)
               case (19)
                   read(unit_oldMenu,format_line) dum
                   write(unit_no,format_icc) (icc(i),i=1,12),(l)
                   write(unit_menu,format_icc) (icc(i),i=1,12),(l)
           end select
           write(unit_no,format_adjustment) debugRes,' SUCCESSFULLY PROCESSED LINE ', line, ' & HRU # ',l
           l = l + 1
           line = line + 1
    701 end do

    end subroutine initializeline

end module m_acru_menu
