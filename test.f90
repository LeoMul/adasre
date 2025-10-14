program main 
    use variables
    implicit none 

    !indexed according to lv ... could be a bad idea.
    call input
    open(25,file='debug')

    call readoic('o1',.true.)
    call readoic('o2',.false.)

    open(30,file='adf04standin')
    !print*,'hello',numberContinuumSave
    write(30,'(10X,30ES10.2)') temps
    do ii = 1,numberContinuumSave 
        do jj = ii+1,numberContinuumSave
            write(30,'(2I5,30ES10.2)') jj,ii,upsilon(:,ii,jj)
        end do 
    end do 
    close(30)
    close(25)

end program