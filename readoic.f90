subroutine readoic(file,core)
    use variables
    character*2 :: file
    
    logical :: eof,core
    logical :: form 
    form = .false.

    eof = .false.
    open(1,file=file,action='read',form='unformatted')
    print*,'inside file = ',file,eof
    blknum = 0
    do while(.not. eof)

        if (form) then 
            read(1,*)
        else 
            read(1)
        end if 

        call readblockform(eof,core,blknum,form)
    end do 
    print*,'finished with file ',file
    close(1)

end subroutine