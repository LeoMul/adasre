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
    if(allocated(upsilon)) deallocate(upsilon) 
    call deallocateReadInArrays 
    close(26) 
    close(25)
    close(90)
    close(99)
end subroutine