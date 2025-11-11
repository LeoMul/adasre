subroutine postProcess
    !reads all of the found oic files and find the upsilon contribution. 
    logical :: readCore ,firstread
    logical :: exists_form ,exists_unform
    integer :: fileiter
    character*2 :: oicFilesForm(9)
    data oicFilesForm/ 'o1','o2','o3','o4','o5','o6','o7','o8','o9' / 
    character*3 :: oicFilesUnForm(9)
    data oicFilesUnForm/ 'o1u','o2u','o3u','o4u','o5u','o6u','o7u', &
    'o8u','o9u' /  
    
    readCore  = .true. 
    firstread = .true.

    do fileiter = 1,9

        inquire(file=oicFilesForm(fileiter),exist=exists_form)
        if (exists_form) then 
!           
            print 1
            print*,'File ',oicFilesForm(fileiter),' exists.'
!
            call readoic(oicFilesForm(fileiter),readCore,&
                         firstread,.true.)
            print 1
        else 
!
            inquire(file=oicFilesUnForm(fileiter),exist=exists_unform)
            if (exists_unform) then 
                print*,'File ',oicFilesUnForm(fileiter),' exists.'
                call readoic(oicFilesUnForm(fileiter),readCore,&
                            firstread,.false.)
                print 1
            end if  
!
        end if 
!
    end do 
1 FORMAT('-------------------------------------------------------------')
end subroutine postProcess