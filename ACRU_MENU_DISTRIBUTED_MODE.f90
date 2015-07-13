
	INTEGER HRUNum,COUNTER,COUNT2 
	CHARACTER*129 INFILE, OUTFILE, DUM, LINE

      write(*,*)'******************************************************'
	write(*,*)'              MAKE_SNOWMANY_MANYHRU.F'
	write(*,*)'               by Stefan W. Kienzle'
	write(*,*)'                   27 Nov 2012'
      write(*,*)'  '
      write(*,*)'  Reads a specific format ACRU MENU file and converts'
      write(*,*)'  it into an apprpriate MENU for distributed mode'
	write(*,*)''
	write(*,*)'            Your input file MUST be:'
	write(*,*)'      C:\AAHMS\Data_Processing\MENU_ONE_HRU'
      write(*,*)' '
      write(*,*)'******************************************************'

C==========================================================================
	Write(*,*)' *****************************************************'
	Write(*,*)' Key in number of HRUs to be in the MENU' 
	Write(*,*)' *****************************************************'
      CALL GETARG(1,DUM)      
      READ(*,'(i)') HRUNum      

	write(*,*) 'HRUNum=',HRUNum    

	INFILE='C:\AAHMS\Data_Processing\MENU_ONE_HRU'
	OPEN(UNIT=11,FILE=INFILE)
      WRITE(*,*) 'Processing Input File:  ',INFILE
	  
	OUTFILE='C:\AAHMS\Data_Processing\MENU_MANY'
	OPEN(UNIT=21,FILE=OUTFILE)
      WRITE(*,*) 'Processing Output File: ',outfile

      COUNT2=0
	COUNTER=0

  100    FORMAT(A80)

c     Copy first 18 lines

      DO 700 WHILE (COUNTER.LT.18)
	   COUNTER=COUNTER+1
         READ(11,100,END=999)DUM
         WRITE(21,100)DUM	   
  700 CONTINUE    

      DO 800 WHILE (COUNT2.LT.147)
	   COUNTER=0
	   COUNT2=COUNT2+1

         READ(11,100,END=999)DUM
         WRITE(21,100)DUM

         READ(11,100,END=999)DUM
         WRITE(21,100)DUM

         READ(11,100,END=999)DUM
         WRITE(21,100)DUM

         READ(11,100,END=999)DUM
         WRITE(21,100)DUM

         READ(11,100,END=999)LINE

         DO 900 WHILE (COUNTER.LT.HRUNum)
	      COUNTER=COUNTER + 1
            WRITE(21,101)LINE,COUNTER
  101       FORMAT(A76,I4.0)
  900    CONTINUE 

         READ(11,100,END=999)DUM
         WRITE(21,100)DUM  

         WRITE(*,*)'Processed ACRU MENU ITEM ',COUNT2,' out of ',HRUNum

  800 CONTINUE
  999 WRITE(*,*) '****************************************************'
	WRITE(*,*) '         PROGRAM SUCCESSFULLY FINISHED       ' 
	WRITE(*,*) ' '
	WRITE(*,*) '                 The outfile is: '
	WRITE(*,*) ' '	 
	WRITE(*,*) '      C:\AAHMS\Data_Processing\MENU_MANY'
	WRITE(*,*) ''
	WRITE(*,*) '****************************************************'
      PAUSE

      STOP
      end