      PROGRAM evtstd
C...Extract parton-level events according to Les Houches Accord.
C...The Les Houches Event File produced here can then be used  
C...as input for hadron-level event simulation, also in Pythia8.

C...Note: you need to create two temporary files for MSTP(161) and MSTP(162). 
C...The final call to PYLHEF will pack them into a standard-compliant 
C...Les Houches Event File on your unit MSTP(163), and erase the two
C...temporary files (unless you set MSTP(164)=1).

C...IMPORTANT: the PYLHEF routine attached below is necessary if you run
C...with PYTHIA versions up to and including 6.403. Starting with 6.404
C...this routine is already part of the standard distribution, so you
C...should remove the copy in this file.

C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP

C...Commonblocks.
C      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
C...The PYTHIA event record:
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
C...PYTHIA MSSM and subprocess common blocks
      COMMON/PYMSSM/IMSS(0:99),RMSS(0:99)
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
C...Random number generator information.
      COMMON/PYDATR/MRPY(6),RRPY(100)
C...Parameters.
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
C...EXTERNAL statement links PYDATA on most machines.
      EXTERNAL PYDATA

      include './pgs.inc'

      integer istr,mstat
      integer nevt,i,nprnt,lok

      character *80 inputfile
      character *80 outputfile

C ..  for HEPEVT
      integer evtnum, numptl 
      integer d1, d2, d3, d4 , d5 , d6, d7
      real d8, d9, d10 
      real d11,d12,d13,d14,d15, d16
      real    x, y 
      integer nmax , u 
      parameter (nmax =100000, u=20 ) 
      
      if(iargc().ne.2) then 
         write(*,*) 'hepevt2stdhep inputfile outputfile'
         stop
      endif
      CALL getarg(1,inputfile)
      CALL getarg(2,outputfile)

      OPEN(u,FILE=inputfile,STATUS='unknown')
 
 
 

      istr=27
C      OPEN(istr, FILE='slha.stdhep',STATUS='unknown')
      call stdxwinit(outputfile, 'StdHEP', nevt, istr, lok)
      call stdflpyxsec(nevt)
      call stdxwrt(100,istr,lok)

C...Initialize.
c      CALL PYINIT('NONE',' ',' ',0D0)

C...Event loop. List first few events.
      DO 200 IEV=1,nmax
c        CALL PYEVNW
         read(u,*) evtnum , numptl 
         write(*,*) 'evtnum = ', evtnum, 'numptl =', numptl

         
         do 10 i = 1 , numptl 
            read(u,1000)  d1 , d2 , d3 , d4 , d5 , d6, d7, 
     $                    d8 , d9 , d10, d11, d12
     $                    d13, d14, d15, d16
c            write(*,*) 'd1 =',d1,', d2 = ' , d2
c            write(*,*) 'd3 =',d3,', d4 = ' , d4
c            write(*,*) 'd5 =',d5,', d6 = ' , d6
c            write(*,*) 'd7 =',d7,', d8 = ' , d8
c            write(*,*) 'd9 =',d9,', d10= ' , d10
c            write(*,*) 'd11=',d11,', d12= ' , d12
c            write(*,*) 'd13=',d13,', d14= ' , d14
c            write(*,*) 'd15=',d15,', d16= ' , d16

            NEVHEP = evtnum 
            NHEP = numptl
            ISTHEP(i) =  d2
            IDHEP(i) = d3
            JMOHEP(1,i) = d4  
            JMOHEP(2,i) = d5 
            JDAHEP(1,i) = d6 
            JDAHEP(2,i) = d7 
            PHEP(1,i) = d8
            PHEP(2,i) = d9
            PHEP(3,i) = d10
            PHEP(4,i) = d11
            PHEP(5,i) = d12 
            VHEP(1,i) = d13
            VHEP(2,i) = d14
            VHEP(3,i) = d15
            VHEP(4,i) = d16
 10      enddo


c        CALL PYHEPC(1)

	CALL LUNHEP(2)
c        WRITE(*,*) 'test=',K(1,2) 


        CALL HEPLST(1)

        CALL PYLIST(1)

       call stdxwrt(1,istr,lok)
c	WRITE(*,*) i
C        if(i.le.nprnt)then
C          call pylist(2)
C          call heplst(1)
C        endif
 200  CONTINUE

      call stdflpyxsec(NEVT)
      call stdxwrt(200,istr,lok)
      call stdxend(istr)

C...Final statistics.
C      CALL PYSTAT(1)
C      CALL PYSTAT(2)
      stop
 1000 FORMAT (i5,i4,i7,i4,i4,i4,i4,9E16.8)

      END
      subroutine pgs_user_pythia
      return
      end

      subroutine pgs_user_event
      return
      end

      subroutine pgs_user_herwig
      return
      end

