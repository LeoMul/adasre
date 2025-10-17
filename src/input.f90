subroutine input()
    !
    !Checks for, and reads in the input deck. 
    !Within in the input deck should be the LEVELS output from the 
    !{\sc autostructure} structure run.  
    !
    use variables
    implicit none 
    integer     :: iii 
    logical     :: inputExists
    character*5 :: inputfile   = 'input'
    namelist /adasre/ numtot,nmax,initresdim
!   Initialize defaults 
    numtot     = 0 
    nmax       = 0
    initresdim = 0  
!   Check for the input
    inquire(file=inputfile,exist=inputExists)
!
    if (.not. inputExists) then    
        write(25,*) 'No input detected!'
        write(99,*) 'No input detected!'
        stop        'No input detected!' 
    end if
!   If it exists - read the file 
    open(50,file=inputfile)
!   read namelist
    read(50,adasre)
!   Deal with defaults
    if (numtot     .eq. 0) stop 'numtot = 0 - no input data'
    if (nmax       .eq. 0) nmax       = numtot 
    if (initresdim .eq. 0) initresdim = numresdefault
!   All of the stuff from the LEVELS file.
    ntar1 = numtot
    numres = initresdim
    allocate(energyFromInput(ntar1))
    allocate(angJFromInput  (ntar1))
    allocate(angPFromInput  (ntar1))
    allocate(angSFromInput  (ntar1))
    allocate(angLFromInput  (ntar1))
    allocate(cfNumFromInput (ntar1))
!   Read in
    do iii = 1,ntar1
        read(50,'(I2,I2,I4,I2,I5,5X,F18.8)')  angJFromInput(iii),&
                                              angPFromInput(iii),&
                                              angSFromInput(iii),&
                                              angLFromInput(iii),&
                                             cfNumFromInput(iii),&
                                            energyFromInput(iii)
    end do  
!   Need this to correctly place the continuum energies with respect
!   to the resonances
    read(50,'(20X,F18.8)') groundFromInput 
!
    if(groundFromInput.gt.0) then 
        open (62,file='istop')
        write(62,*)'invalid ground state of input energies'
        close(62)
        stop       'invalid ground state of input energies'
    end if 
!
    energyFromInput = energyFromInput + groundFromInput
!
    close (50)
 !   
end subroutine input