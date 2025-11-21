subroutine init 
    use variables 
    implicit none
    open(90,file='corecounting')
    open(25,file='debug')
    open(26,file='memoryTracking')
    open(99,file='output')
    call initReadInArrays
end subroutine

subroutine deinit
    use variables
    implicit none
    call deallocateReadInArrays 
    if(allocated(upsilon))         deallocate(upsilon) 
    if(allocated(drrate))          deallocate(drrate) 
    if(allocated(collstengthData)) deallocate(collstengthData) 
    if(allocated(energyGrid))      deallocate(energyGrid)
    if(allocated(lorentzarray))    deallocate(lorentzarray)
    close(26) 
    close(25)
    close(90)
    close(99)
end subroutine