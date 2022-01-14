!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: BUFR_TRANSAPHIR
!   PRGMMR: Y. LING             ORG: NP22        DATE: 2017-03-06
!
! ABSTRACT: READS IN MEGHA-TROPIQUES SAPHIR L1A2 BRIGHTNESS TEMPERATURE DATA 
! IN WMO BUFR FORMAT, AND REPACK IT INTO AN NCEP BUFR FILE WHICH INCLUDES RSRD
! AND EXPRSRD
!
! PROGRAM HISTORY LOG:
! 2016-06-10  Y. Ling - Original author
! 2017-03-06  Y. Ling - Change to compress BUFR messages in output file. Will
!                       save a lot of space. (Input files are not compressed.)
!
! USAGE:
!   INPUT FILES:
!     UNIT 05  - STANDARD INPUT. W3TRNARG PARSES ARGUMENTS FROM
!              - STANDARD INPUT.
!     UNIT 11  - WMO BUFR FILE.
!     UNIT 19  - FOREIGN BUFR TABLE FILE CONTAINING BUFR TABLES A,
!              - B, AND D (FOR UNIT 11).
!     UNIT 20  - NCEP BUFR TABLE FILE CONTAINING BUFR TABLES A, B, AND
!                D (FOR UNIT 51).
!
!   OUTPUT FILES:
!     UNIT 06  - PRINTOUT
!     UNIT 51  - POINTS TO THE OUTPUT BUFR FILE. TRANJB WILL PLACE
!                THE BUFR MESSAGES INTO THE PROPER TANKS.
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!       W3NCO  - W3TRNARG W3TAGB   W3TAGE   ERREXIT
!     BUFRLIB  - OPENBF   CLOSBF   OPENMB   UFBINT   UFBREP   WRITCP
!              - IREADMG  IREADSB  DATELEN
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!          =   1 - UNABLE TO PARSE INPUT ARGUMENTS IN W3TRNARG
!          = 253 - NO REPORTS WRITTEN OUT
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 95
!   MACHINE:  NCEP WCOSS
!
!$$$

program BUFR_TRANSAPHIR
implicit none

real(8),dimension(92) :: seqdata_in
real(8),dimension(94) :: seqdata_out
integer :: lunin,lindx,lundx,lunot,ireadmg,ireadsb 
integer :: iret1,iret2,idate,ldate,ii,msg_count 
integer :: lsubdr,ltnkid,lapchr,jdate,kdate,ierr 

character(8) :: subset,subfgn,tlflag
character(80) :: appchr,subdir,tankid
character(60) :: seqdatastr

lunin=11
lindx=19
lundx=20
lunot=51
!subset='NC021242'

call w3tagb('BUFR_TRANSAPHIR',2017,0065,0084,'NP22')

PRINT *, ' '
PRINT *, ' ==> Welcome to BUFR_TRANSAPHIR -- Version 03/06/2017'
PRINT *, ' '

call w3trnarg(subdir,lsubdr,tankid,ltnkid,appchr,lapchr,tlflag,jdate,kdate,ierr)
if(ierr /= 0) then
   write(6,&
   '(''UNABLE TO PARSE ARGS TO TRANSLATION ROUTINE - RETURN CODE = '',I5)') IERR
   call  w3tage('BUFR_TRANIMGR')
   call  errexit(ierr)
end if

subset = 'NC'//subdir(lsubdr-2:lsubdr)//tankid(ltnkid-2:ltnkid)
!------------------------------------------------------------------------------

seqdata_out=10e10
msg_count=0

call datelen(10)

!  Open the input and output BUFR files
!  ------------------------------------
call openbf(lunin,'IN',lindx)
call openbf(lunot,'NODX',lundx)

!  Read through the message/subsets in the file
!  --------------------------------------------
do while(ireadmg(lunin,subfgn,idate) .eq. 0)

!print *, 'idate =', idate
!print *,' subfgn is ',subfgn

 do while(ireadsb(lunin) .eq. 0)
    seqdata_in=10e10
    seqdata_out=10e10

    call ufbseq(lunin,seqdata_in,92,1,iret1,subfgn)

    seqdata_out(1)=256
    do ii=1,92
       seqdata_out(ii+2)=seqdata_in(ii) 
    end do

!    seqdata_out(93)=256
    ldate=idate
!    print *, 'now print seqdata_out:'
!    print *, seqdata_out

    call openmb(lunot,subset,ldate)
!   Write out subset
    call ufbseq(lunot,seqdata_out,94,1,iret2,subset)
    call WRITCP(lunot)

    msg_count=msg_count+1

   enddo
enddo
   
call closbf(lunin)
call closbf(lunot)

print *, '*** PROCESSING ENDED NORMALLY ***'
print*,'*** MESSAGE PROCESSED: ',msg_count
print*,'*** PROCESSING ENDED NORMALLY ***'
      
if(msg_count == 0) then
   print *, 'NO REPORTS PROCESSED -- DISABLING ALL SUBSEQUENT PROCESSING.'
   CALL W3TAGE('BUFR_TRANSAPHIR')
   CALL ERREXIT(253)
end if

CALL W3TAGE('BUFR_TRANSAPHIR')

stop
end
