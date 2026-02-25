subroutine writeout_aa(nlev,eres,configPointer,tg,ntcont,aa) 
    use variables
    use configs
    !todo, have it mark which Rydberg series each resonance comes from. 
    !Core marked zero, perhaps. 
    implicit none 
    integer :: nlev,ntcont
    real*8  :: aa(ntcont,nlevels)
    real*8  :: eres(nlevels),tg
    integer :: configPointer(nlevels)
   ! real*8  :: branching_ratio(nlevels,ncont)
   ! real*8  :: aa(nlevels,ncont) 
    integer :: ii ,jj 
    character*4 :: char4 
    rydbergLabel = ''
    do ii = 1, ncontium_cf_UNIQUE 
        offset = 0 
        do jj =1 , LMX_CONT_UNIQUE(ii)
            write(char4,'(I2,A,A)') princN(qlb(ii,jj)),&
                                              LLAB  (orbl(qlb(ii,jj))),&
                                              QSB(ii,jj)
            rydbergLabel(ii)(1+offset:4+offset) = char4
            offset = offset + 4 
        end do 
        write(char4,'(A2,A,A)') ' N','L','1'
        rydbergLabel(ii)(1+offset:4+offset) = char4
    end do 

    !write(64,*) blknum
    !write(0,*) 'i am in writeout_aa',blknum



    do ii = 1, nlev 
        if ( (configPointer(ii) > 0) .and. (configPointer(ii) .le. ncontium_cf_UNIQUE)) then 
            write(64,'(f15.8,a,es15.8,a,a23,a,i6)') eres(ii) - groundFromInput ,',',aa(1,ii) ,',',rydbergLabel(configPointer(ii)) ,',',configPointer(ii)
        end if
    end do 

end subroutine  


