subroutine postProcess
    !reads all of the found oic files and find the upsilon contribution. 
    logical :: readCore ,firstread
    logical :: exists ,exists2
    integer :: fileiter
    character*2 :: oicFilesForm(9)
    data oicFilesForm/ 'o1','o2','o3','o4','o5','o6','o7','o8','o9' / 
    character*3 :: oicFilesUnForm(9)
    data oicFilesUnForm/ 'o1u','o2u','o3u','o4u','o5u','o6u','o7u', &
    'o8u','o9u' /  
    
    readCore  = .true. 
    firstread = .true.

    do fileiter = 1,9

        inquire(file=oicFilesForm(fileiter),exist=exists)
        if (exists) then 
            print*,oicFilesForm(fileiter),'exists'
            call readoic(oicFilesForm(fileiter),readCore,&
                         firstread,.true.)
        else 

            inquire(file=oicFilesUnForm(fileiter),exist=exists2)
            if (exists2) then 
                print*,oicFilesUnForm(fileiter),'exists'
                call readoic(oicFilesUnForm(fileiter),readCore,&
                            firstread,.false.)
            end if  

        end if 

    end do 

end subroutine postProcess

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

end subroutine readoic 