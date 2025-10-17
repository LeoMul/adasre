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
    read(50,*) ntar1 
!   All of the stuff from the LEVELS file.
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
    energyFromInput = energyFromInput + groundFromInput
!
    close (50)
 !   
end subroutine input