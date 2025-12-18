subroutine input()
    !
    !Checks for, and reads in the input deck. 
    !Within in the input deck should be the LEVELS output from the 
    !{\sc autostructure} structure run.  
    !
    use variables
    use tempgrid
    implicit none 
    integer     :: uu, ll 
    integer     :: iii 
    logical     :: inputExists
    integer     :: iostat
    integer     :: temp 
    character*5 :: inputfile   = 'input'
    integer     :: idamp 
    namelist /adasre/ numtot,nmax,initresdim,calcdr,collstreng,minERyd, & 
    maxERyd,collstrengnpoints, temp,idamp
!
    !   Initialize defaults 
    numtot     =  0 
    nmax       =  0
    initresdim =  0  
    calcdr     =  0 
    includerad =  0 
    collstreng =  0 
    temp       =  0
    idamp      =  0
    damp = .false.
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
    if (numtot     .eq.  0) stop 'numtot = 0 - no input data'
    if (nmax       .eq.  0) nmax       = numtot 
    if (initresdim .eq.  0) initresdim = numresdefault
    if (idamp      .NE.  0) damp = .true.
    ! temp grid 
    if (temp == 0) then 
        call defaulttempgrid 
    else if (temp == -1) then
        call knetempgrid
    else 
        call readtempgrid(temp) 
    end if 
    
    write(25,*) 'proceeding with ',numtot,nmax,initresdim
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
    if (collstreng > 0) then 
        allocate( collstrenglower(collstreng) )
        allocate( collstrengupper(collstreng) )
        do iii = 1, collstreng 
            read(50, *,iostat=iostat) uu,ll 
            if (iostat .ne. 0) then 
                write(25,*) 'Unexpected end of requested OMEGAs '
                write(99,*) 'Unexpected end of requested OMEGAs '
                stop        'Unexpected end of requested OMEGAs '             
            end if 
            collstrengupper(iii) = max(uu,ll)
            collstrenglower(iii) = min(uu,ll)
        end do  

        allocate( energyGrid(collstrengnpoints)) 
        deltaERyd = ( maxERyd - minERyd )  / real(collstrengnpoints,8) 
        energyGrid(1) = minERyd  
        do iii = 2, collstrengnpoints
            energyGrid(iii) = energyGrid(iii-1) + deltaERyd 
        end do
        allocate (collstengthData(collstreng,collstrengnpoints)) 
        allocate (lorentzarray(collstrengnpoints))
        collstengthData = 0.0d0 
    end if 

    !
    close (50)
 !   
end subroutine input