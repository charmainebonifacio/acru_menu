
	INTEGER HRUNum,COUNTER,COUNT2,HRUtot,HRU,REST 
	CHARACTER*90 INFILE, OUTFILE, LINE, DUM
	INTEGER HRUFirst,HRULast
      write(*,*)'******************************************************'
	write(*,*)'              MAKE_ACRUMENU_Selected_HRUs.f'
	write(*,*)'               by Stefan W. Kienzle'
	write(*,*)'                   05 Oct 2009'
      write(*,*)'  '
      write(*,*)'    Reads an ACRU MENU file and selects those HRUs'
	write(*,*)'             based on lowest and highest HRUs.'
      write(*,*)' '
	write(*,*)'** MENU MUST BE called MENU AND BE IN THIS FOLDER:**'
	write(*,*)'            c:\AAHMS\Data_Processing\'
      write(*,*)' '
      write(*,*)'******************************************************'

C==========================================================================
	Write(*,*)' *****************************************************'
	Write(*,*)' Key in first HRU# to process, e.g. 1'
	Write(*,*)' *****************************************************'
      CALL GETARG(1,DUM)
      READ(*,'(i)') HRUFirst
c	HRUFirst = DUM
 	write(*,*) 'HRUFirst=',HRUFirst

C==========================================================================

	Write(*,*)' *****************************************************'
	Write(*,*)' Key in last HRU# to process, e.g. 27'
	Write(*,*)' *****************************************************'
      CALL GETARG(1,DUM)
      READ(*,'(i)') HRULast
 	write(*,*) 'HRULast=',HRULast

C==========================================================================
	Write(*,*)' *****************************************************'
	Write(*,*)' Key in total number of HRUs in the Input MENU' 
	Write(*,*)' *****************************************************'
      CALL GETARG(1,DUM)      
      READ(*,'(i)') HRUNum
	write(*,*) 'HRUNum = ',HRUNum    

C==========================================================================
	INFILE='C:\AAHMS\Data_Processing\MENU'
	OPEN(UNIT=11,FILE=INFILE)

      OUTFILE='C:\AAHMS\Data_Processing\MENU_Selected_HRUs'
	OPEN(UNIT=21,FILE=OUTFILE)
      WRITE(*,*) 'Processing Output File: ',outfile

C==========================================================================
	COUNTER=0
	COUNT2=1

  100 FORMAT(A80)

C==========================================================================
c     Copy first 17 lines

      DO 700 WHILE (COUNTER.LT.17)
	   COUNTER=COUNTER+1
         READ(11,100,END=999)LINE
         WRITE(21,100)LINE	   
  700 CONTINUE    

C==========================================================================
c     Write only selected HRU to output file

      DO 800 WHILE (COUNT2.LT.147)
	   COUNT=COUNT+1
	   COUNTER=0

c     Read Header lines
         READ(11,100,END=999)LINE
         WRITE(21,100)LINE

         READ(11,100,END=999)LINE
         WRITE(21,100)LINE

         READ(11,100,END=999)LINE
         WRITE(21,100)LINE

         READ(11,100,END=999)LINE
         WRITE(21,100)LINE

         READ(11,100,END=999)LINE
         WRITE(21,100)LINE

c        Read all HRUs, but only write selected ones 
             DO 801 WHILE (COUNTER.LT.HRUNum)
	          COUNTER=COUNTER+1
                READ(11,100,END=999)LINE
      
	          IF(COUNTER.GE.HRUFirst.AND.COUNTER.LE.HRULast) THEN
			     WRITE(21,100)LINE
                ENDIF

  801        CONTINUE

         WRITE(*,*)'Processed ACRU MENU ITEM ',COUNT2,' out of 147'
         COUNT2=COUNT2+1
  800 CONTINUE

  999 WRITE(*,*)'****************************************************'
	WRITE(*,*)'          PROGRAM SUCCESSFULLY FINISHED       ' 
      write(*,*)'  '
      write(*,*)'            The output file is called:'
      write(*,*)'  '	
      write(*,*)outfile
	write(*,*)''
	write(*,*)'           Please rename this file NOW !!'
	write(*,*)'           ------------------------------'
      write(*,*)'  ' 
	WRITE(*,*)'****************************************************'

      PAUSE
      STOP
      end