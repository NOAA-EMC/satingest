  program temp_readmp
  character(200) file
  character*8 subset
  real(8) gcrchn,tab(3,200000)
  logical exist
  iret=isetprm('MAXSS',1500000)

! Jack Woolen - test incoming NESDIS bufr files
! check if file exists, then open it, else abort
  call getarg(1,file)
  file = trim(adjustl(file))
  inquire(file=file,exist=exist)
  if (.not.exist) call bort(trim(file)//' does not exist')
  open(20,file=file,form='unformatted')

  call openbf(20,'IN',20)
  do while(ireadmg(20,subset,idate).eq.0)
  do while(ireadsb(20).eq.0)
  call ufbcnt(20,irec,isub)
  call ufbint(20,GCRCHN,1,1,iret,'{GCRCHN}');
  enddo
  enddo
  end program
