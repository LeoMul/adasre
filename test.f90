program main 
    use variables
    use configs
    use reference_data
    implicit none 

    !indexed according to lv ... could be a bad idea.
    call input
    
    open(25,file='debug')

    call readoic('o1',.true.)
    call readoic('o2',.false.)

    open(30,file='adf04standin')
    !print*,'hello',numberContinuumSave
    !print*,ncontium_cf_UNIQUE
    !print*,LMX_CONT
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
    write(30,'(I5)') -1 

    close(30)
    close(25)

    !open(45,file='energies')
    !do ii = 1 , numberContinuumSave
    !    write(45,*) energyNstates(ii),energyFromInput(ii)-groundFromInput
    !end do 
    !close(45)

end program