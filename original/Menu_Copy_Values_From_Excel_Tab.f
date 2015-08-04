      USE DFLIB
      INTEGER LINE,I,J,K,L,M,N,O,P
	INTEGER ISUBNO,LINECOIAM,LINECAY,LINEELAIM,LINEROOTA
	INTEGER LINEICC,LINEALBEDO,ICONS,ISWAVE 
	REAL D1,D2
	DIMENSION COIAM(12),CAY(12),ELAIM(12),ROOTA(12)
	DIMENSION ICC(12),ALBEDO(12)

	CHARACTER*200 IFILE1,IFILE2,OUTFILE,DUM,OUT,KILL
	LOGICAL(4) result

      write(*,*)'******************************************************'
	write(*,*)'         MENU_COPY_VALUES_FROM_EXCEL_TAB.F'
	write(*,*)'               by Stefan W. Kienzle'
	write(*,*)'                   19. May. 2008'
	write(*,*)'         updated   22. June 2014'
      write(*,*)'  '
      write(*,*)'  Copy COIAM,CAY,ELAIM,ROOTA,ICC,ALBEDO ' 
	write(*,*)'  from Excel Tab delimited format to the MENU file'
      write(*,*)' '
      write(*,*)'******************************************************'

      OUT='copy c:\AAHMS\Castle__\MENU c:\AAHMS\Castle__\MENU_OLD'
      result=SYSTEMQQ(out)
      write(*,*) 'Copied File MENU to MENU_OLD'
      KILL='del c:\AAHMS\Castle__\MENU'
	result=SYSTEMQQ(kill)
      write(*,*) 'Deleted File MENU' 

      IFILE1='C:\AAHMS\Castle__\MENU_INFO1.txt'

      IFILE2='C:\AAHMS\Castle__\MENU_OLD'
      OPEN(UNIT=12,FILE=IFILE2)

      OUTFILE='C:\AAHMS\Castle__\MENU'
	OPEN(UNIT=20,FILE=OUTFILE)

      P=1
      DO 400 WHILE (P.LT.11)
         READ(12,111)dum
	   P=P+1
  400 CONTINUE

      READ(12,112)ISUBNO
  112 FORMAT(3X,I4)
      CLOSE(12)

      WRITE(*,*) 'Number of HRUs = ',ISUBNO

      OPEN(UNIT=12,FILE=IFILE2)

	LINEALBEDO=23+((ISUBNO+5)*28)
      LINECAY   =23+((ISUBNO+5)*53)
      LINEELAIM =23+((ISUBNO+5)*54)
	LINEROOTA =23+((ISUBNO+5)*56)
      LINECOIAM =23+((ISUBNO+5)*67)
	LINEICC   =23+((ISUBNO+5)*141)



      LINE=1

      DO 900 WHILE (LINE.LT.10000000)
	   L=1
	   READ(12,111,END=999)DUM
  111    FORMAT(A200)


C     CHECK WHERE ALBEDO SHOULD BE OVERWRITTEN  =================================
         IF(LINE.EQ.LINEALBEDO) THEN
               OPEN(UNIT=11,FILE=IFILE1)
               READ(11,111)DUM
	         READ(11,111)DUM
            DO 901 WHILE (L.LE.ISUBNO)
               READ(11,*)D1,D2,
     &                   (COIAM(I),I=1,12),(CAY(I),I=1,12),
     &                   (ELAIM(I),I=1,12),(ROOTA(I),I=1,12),
     &                   (ICC(I),I=1,12),(ALBEDO(I),I=1,12) 

c      read original MENU ICONS and ISWAVE variables
               READ(12,113)ICONS,ISWAVE
  113 FORMAT(66X,I1,5X,I1)

	         WRITE(20,222)(ALBEDO(I),I=1,12),ICONS,ISWAVE,L
  222          FORMAT(1X,11(F4.2,' '),F4.2,6X,I1,5X,I1,3X,I4)

C     This needs to be done for the Orari watershed, because HRU=1 does not exist:
c	         IF(LINE.EQ.LINEALBEDO) THEN
c			      WRITE(20,221)(ALBEDO(I),I=1,12),L+1
c                    L=L+1
c	              LINE=LINE+1
c               write(*,*) 'Processing Line ',LINE, 'Overwriting ALBEDO'
c                    READ(12,111)DUM
c	         ENDIF

               L=L+1
	         LINE=LINE+1
               write(*,*) 'Processing Line ',LINE, 'Overwriting ALBEDO'
  901       CONTINUE
            CLOSE(11)
         ENDIF
C     END CHECK WHERE ALBEDO SHOULD BE OVERWRITTEN   ===========================


C     CHECK WHERE COIAM SHOULD BE OVERWRITTEN   =========================
         IF(LINE.EQ.LINECOIAM) THEN
               OPEN(UNIT=11,FILE=IFILE1)
               READ(11,111)DUM
	         READ(11,111)DUM
            DO 902 WHILE (L.LE.ISUBNO)
               READ(11,*)D1,D2,
     &	               (COIAM(I),I=1,12),(CAY(I),I=1,12),
     &                   (ELAIM(I),I=1,12),(ROOTA(I),I=1,12),
     &                   (ICC(I),I=1,12),(ALBEDO(I),I=1,12) 
               READ(12,111)DUM
	         WRITE(20,221)(COIAM(I),I=1,12),(L)
  221 FORMAT(1X,12(F4.2,','),15X,I4)

C     This needs to be done for the Orari watershed, because HRU=1 does not exist:
c	         IF(LINE.EQ.LINECOIAM) THEN
c			      WRITE(20,221)(COIAM(I),I=1,12),L+1
c                    L=L+1
c	              LINE=LINE+1
c               write(*,*) 'Processing Line ',LINE, 'Overwriting COIAM'
c                    READ(12,111)DUM
c	         ENDIF


               L=L+1
	         LINE=LINE+1
               write(*,*) 'Processing Line ',LINE, 'Overwriting COIAM'
  902       CONTINUE
            CLOSE(11)
	
         ENDIF
	
C     END CHECK WHERE COIAM SHOULD BE OVERWRITTEN   =========================

C     CHECK WHERE CAY SHOULD BE OVERWRITTEN   ===============================
         IF(LINE.EQ.LINECAY) THEN
               OPEN(UNIT=11,FILE=IFILE1)
               READ(11,111)DUM
	         READ(11,111)DUM
            DO 903 WHILE (L.LE.ISUBNO)
               READ(11,*)D1,D2,
     &                   (COIAM(I),I=1,12),(CAY(I),I=1,12),
     &                   (ELAIM(I),I=1,12),(ROOTA(I),I=1,12),
     &                   (ICC(I),I=1,12),(ALBEDO(I),I=1,12) 
               READ(12,111)DUM
	         WRITE(20,221)(CAY(I),I=1,12),(L)
C  221 FORMAT(1X,12(F4.2,','),5X,'*****',5X,I3)

C     This needs to be done for the Orari watershed, because HRU=1 does not exist:
c	         IF(LINE.EQ.LINECAY) THEN
c			      WRITE(20,221)(CAY(I),I=1,12),L+1
c                    L=L+1
c	              LINE=LINE+1
c                    write(*,*) 'Processing Line ',LINE,'Overwriting CAY'
c                    READ(12,111)DUM
c	         ENDIF

               L=L+1
	         LINE=LINE+1
               write(*,*) 'Processing Line ',LINE, 'Overwriting CAY'
  903       CONTINUE
            CLOSE(11)
         ENDIF
	
C     END CHECK WHERE CAY SHOULD BE OVERWRITTEN   ==========================

C     CHECK WHERE ELAIM SHOULD BE OVERWRITTEN  =============================
         IF(LINE.EQ.LINEELAIM) THEN
               OPEN(UNIT=11,FILE=IFILE1)
               READ(11,111)DUM
	         READ(11,111)DUM
            DO 904 WHILE (L.LE.ISUBNO)
               READ(11,*)D1,D2,
     &                   (COIAM(I),I=1,12),(CAY(I),I=1,12),
     &                   (ELAIM(I),I=1,12),(ROOTA(I),I=1,12),
     &                   (ICC(I),I=1,12),(ALBEDO(I),I=1,12) 
               READ(12,111)DUM
	         WRITE(20,221)(ELAIM(I),I=1,12),(L)
C  221 FORMAT(1X,12(F4.2,','),5X,'*****',5X,I3)

C     This needs to be done for the Orari watershed, because HRU=1 does not exist:
c	         IF(LINE.EQ.LINEELAIM) THEN
c			      WRITE(20,221)(ELAIM(I),I=1,12),L+1
c                    L=L+1
c	              LINE=LINE+1
c               write(*,*) 'Processing Line ',LINE, 'Overwriting ELAIM'
c                    READ(12,111)DUM
c	         ENDIF

               L=L+1
	         LINE=LINE+1
               write(*,*) 'Processing Line ',LINE, 'Overwriting ELAIM'
  904       CONTINUE
            CLOSE(11)
         ENDIF
	
C     END CHECK WHERE ELAIM SHOULD BE OVERWRITTEN   =============================

C     CHECK WHERE ROOTA SHOULD BE OVERWRITTEN  ==================================
         IF(LINE.EQ.LINEROOTA) THEN
               OPEN(UNIT=11,FILE=IFILE1)
               READ(11,111)DUM
	         READ(11,111)DUM
            DO 905 WHILE (L.LE.ISUBNO)
               READ(11,*)D1,D2,
     &                   (COIAM(I),I=1,12),(CAY(I),I=1,12),
     &                   (ELAIM(I),I=1,12),(ROOTA(I),I=1,12),
     &                   (ICC(I),I=1,12),(ALBEDO(I),I=1,12) 
               READ(12,111)DUM
	         WRITE(20,221)(ROOTA(I),I=1,12),(L)
C  221 FORMAT(1X,12(F4.2,','),5X,'*****',5X,I3)

C     This needs to be done for the Orari watershed, because HRU=1 does not exist:
c	         IF(LINE.EQ.LINEROOTA) THEN
c			      WRITE(20,221)(ROOTA(I),I=1,12),L+1
c                    L=L+1
c	              LINE=LINE+1
c               write(*,*) 'Processing Line ',LINE, 'Overwriting ROOTA'
c                    READ(12,111)DUM
c	         ENDIF

               L=L+1
	         LINE=LINE+1
               write(*,*) 'Processing Line ',LINE, 'Overwriting ROOTA'
  905       CONTINUE
            CLOSE(11)
         ENDIF
	
C     END CHECK WHERE ROOTA SHOULD BE OVERWRITTEN   =============================

C     CHECK WHERE ICC SHOULD BE OVERWRITTEN  ==================================
         IF(LINE.EQ.LINEICC) THEN
               OPEN(UNIT=11,FILE=IFILE1)
               READ(11,111)DUM
	         READ(11,111)DUM
            DO 906 WHILE (L.LE.ISUBNO)
               READ(11,*)D1,D2,
     &                   (COIAM(I),I=1,12),(CAY(I),I=1,12),
     &                   (ELAIM(I),I=1,12),(ROOTA(I),I=1,12),
     &                   (ICC(I),I=1,12),(ALBEDO(I),I=1,12) 
               READ(12,111)DUM
	         WRITE(20,226)(ICC(I),I=1,12),(L)
  226 FORMAT(2X,12(I3,2X),6X,'*****',5X,I4)

C     This needs to be done for the Orari watershed, because HRU=1 does not exist:
c	         IF(LINE.EQ.LINEICC) THEN
c			      WRITE(20,221)(ICC(I),I=1,12),L+1
c                    L=L+1
c	              LINE=LINE+1
c               write(*,*) 'Processing Line ',LINE, 'Overwriting ICC'
c                    READ(12,111)DUM
c	         ENDIF

               L=L+1
	         LINE=LINE+1
               write(*,*) 'Processing Line ',LINE, 'Overwriting ICC'
  906       CONTINUE
            CLOSE(11)
         ENDIF
	
C     END CHECK WHERE ICC SHOULD BE OVERWRITTEN   ============================


C     COPY MENU LINES *************************************************

         WRITE(20,111) DUM

	   WRITE(*,*) 'PROCESSING LINE ',LINE
  899    LINE=LINE+1

  900  CONTINUE
  999 CLOSE(12)
      CLOSE(20)

	WRITE(*,*) 'Program successfully finished.'
	WRITE(*,*) '------------------------------'

      STOP
	END