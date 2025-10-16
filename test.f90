program main 
    use variables
    use configs
    use reference_data
    implicit none 

    logical :: readCore ,firstread
    logical :: exists ,exists2
    integer :: fileiter
    character*2 :: oicFilesForm(9)
    data oicFilesForm/ 'o1','o2','o3','o4','o5','o6','o7','o8','o9' / 
    character*3 :: oicFilesUnForm(9)
    data oicFilesUnForm/ 'o1u','o2u','o3u','o4u','o5u','o6u','o7u','o8u','o9u' / 

    call input
    call initReadInArrays

    readCore  = .true. 
    firstread = .true.
    open(90,file='corecounting')

    open(25,file='debug')

    !call readoic('o1',readCore)
    do fileiter = 1,9

        inquire(file=oicFilesForm(fileiter),exist=exists)
        if (exists) then 
            print*,oicFilesForm(fileiter),'exists'
            call readoic(oicFilesForm(fileiter),readCore,firstread,.true.)
        else 

            inquire(file=oicFilesUnForm(fileiter),exist=exists2)
            if (exists2) then 
                print*,oicFilesUnForm(fileiter),'exists'
                call readoic(oicFilesUnForm(fileiter),readCore,firstread,.false.)
            end if  

        end if 


    end do 
    
    
    open(30,file='adf04standin')
    !print*,'hello',numberContinuumSave
    !print*,ncontium_cf_UNIQUE
    !print*,LMX_CONT

    !todo: move this to an actual subroutine 
    write(30,'(A2,"+",I2,I10,I10,f15.4,"(0Y)")') ELEM2(nzed),NZED-nelec+1,NZED,NZED-nelec+2,0.0d0
    cflabel = ''
    do ii = 1, ncontium_cf_UNIQUE 
        print*,LMX_CONT_UNIQUE(ii)
        offset = 0 
        do jj =1 , LMX_CONT_UNIQUE(ii)
            write(char4,'(I2,A,A)') princN(qlb(ii,jj)),&
                                              LLAB  (orbl(qlb(ii,jj))),&
                                              QSB(ii,jj)
            cflabel(ii)(1+offset:4+offset) = char4
            offset = offset + 4 
        end do 
        !print*,cflabel(ii)
    end do 
    
    
    do ii = 1, numberContinuumSave
        !write(30,*) (energyFromInput(ii)-groundFromInput)*RYDBERGCM,angJFromInput(ii)  
        write(30,'(I5,A19,"(",I1,")",A1,"(",F4.1,")",F21.4)')    II,   &
                                        cflabel(cfNumFromInput(ii)),   &
                                        abs(angSFromInput(ii)),   &
                                        OCCLAB(angLFromInput(ii)),   &
                            0.5d0*angJFromInput(ii),   &
                    (energyFromInput(ii)-groundFromInput)*RYDBERGCM    
    end do 
    write(30,'(I5)') -1 
    write(30,'(" 3.00    5       ")',advance='no')
    do ii = 1,ntemps 
        write(char8,'(ES8.2)') temps(ii)
        write(30,'(A4)',advance='no')    char8(1:4)
        write(30,'(A3,1X)',advance='no') char8(6:8)
    end do 
    write(30,'(1X)')
    
    do ii = 1,numberContinuumSave 
        do jj = ii+1,numberContinuumSave
            !write(30,'(2I4,30ES10.2)') jj,ii,1E-30,upsilon(:,ii,jj),1e-30
            write(30,'(2I4,1X)',advance = 'no') jj,ii
            write(30,'(A7,1X)',advance = 'no') '1.00-30'
            do kk = 1,ntemps
                write(char8,'(ES8.2)') upsilon(kk,ii,jj)
                write(30,'(A4)',advance='no')    char8(1:4)
                write(30,'(A3,1X)',advance='no') char8(6:8)
            end do 
            write(30,'(A7,1X)') '0.00+00'
        end do 
    end do 
    write(30,'(2I5)') -1,-1
    write(30, '(I5)') -1 

    close(30)
    close(25)
    close(90)
    !open(45,file='energies')
    !do ii = 1 , numberContinuumSave
    !    write(45,*) energyNstates(ii),energyFromInput(ii)-groundFromInput
    !end do 
    !close(45)
    deallocate(upsilon)
    call deallocateReadInArrays 

end program