subroutine readoic(file,core,firstread,form)
    use variables
    character(len=*) :: file
    logical,intent(inout) :: core 
    logical,intent(inout) :: firstread
    logical :: eof
    logical :: form 

    eof = .false.
    if (form) then 
        open(1,file=file,action='read',form='formatted')
    else 
        open(1,file=file,action='read',form='unformatted')
    end if 

    print*,'inside file = ',file,eof
    blknum = 0
    do while(.not. eof)

        if (form) then 
            read(1,*)
        else 
            read(1)
        end if 

        call readblockform(eof,core,blknum,form,firstread)
    end do 
    print*,'finished with file ',file
    close(1)

end subroutine