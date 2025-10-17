subroutine init 
    use variables 

    open(90,file='corecounting')
    open(25,file='debug')
    open(26,file='memoryTracking')
    
    call initReadInArrays



end subroutine

subroutine deinit
    use variables
    if(allocated(upsilon)) deallocate(upsilon) 
    call deallocateReadInArrays 
    close(26) 
    close(25)
    close(90)
end subroutine