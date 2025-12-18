subroutine init 
    use variables 
    use omp_lib
    implicit none
    open(90,file='corecounting')
    open(25,file='debug')
    open(26,file='memoryTracking')
    open(99,file='output')
    wallclock1 = omp_get_wtime()
    call initReadInArrays
end subroutine

subroutine deinit
    use variables
    use omp_lib
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
    wallclock2 = omp_get_wtime() 
    
    write(0,*) 'Run time: ', wallclock2 - wallclock1 

end subroutine