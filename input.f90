subroutine input()
    use variables
    implicit none 
    integer :: iii 
    !functions the same way as adasin, for now.
    open(50,file='input')
    read(50,*) ntar1 
    allocate(energyFromInput(ntar1))

    do iii = 1,ntar1
        read(50,'(20X,F18.8)') energyFromInput(iii)
    end do  
    read(50,'(20X,F18.8)') groundFromInput 

    energyFromInput = energyFromInput + groundFromInput

    close (50)
    
end subroutine input