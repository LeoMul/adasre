subroutine readoic(file,core)
    use variables
    character*2 :: file
    
    logical :: eof,core

    eof = .false.
    open(1,file=file,action='read')
    print*,'inside file = ',file,eof

    do while(.not. eof)
        read(1,*)
        call readblockform(eof,core)
    end do 
    close(1)

end subroutine