subroutine input()
    use variables
    implicit none 
    integer :: iii 
    !functions the same way as adasin, for now.
    open(50,file='input')
    read(50,*) ntar1 
    allocate(energyFromInput(ntar1))
    allocate(angJFromInput(ntar1))
    allocate(angPFromInput(ntar1))
    allocate(angSFromInput(ntar1))
    allocate(angLFromInput(ntar1))
    allocate(cfNumFromInput(ntar1))


    do iii = 1,ntar1
        read(50,'(I2,I2,I4,I2,I5,5X,F18.8)')  angJFromInput(iii),&
                                              angPFromInput(iii),&
                                              angSFromInput(iii),&
                                              angLFromInput(iii),&
                                             cfNumFromInput(iii),&
                                             energyFromInput(iii)
    end do  


    read(50,'(20X,F18.8)') groundFromInput 

    energyFromInput = energyFromInput + groundFromInput

    close (50)
    
end subroutine input