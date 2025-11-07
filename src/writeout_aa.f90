subroutine writeout_aa(nlevels,ncont,eres,aa,branching_ratio,blknum,&
    cftracker) 
    implicit none 
    integer :: nlevels, ncont 
    real*8  :: eres(nlevels)
    real*8  :: branching_ratio(nlevels,ncont)
    real*8  :: aa(nlevels,ncont) 
    integer :: blknum 
    integer :: cftracker(nlevels)
    integer :: ii ,jj 
    character*4 :: filename 

    write(filename,'(A3,I1)') 'res',blknum

    open(64,file=filename)

    do ii = 1, nlevels 
        do jj = 1 ,ncont 
            if (aa(ii,jj) > 0.0d0)then 
            write(64,'(3I5,f15.8,2ES10.2)') cftracker(ii),ii,jj, eres(ii), aa(ii,jj),branching_ratio(ii,jj)
            end if 
        end do 
    end do 

    close(64)
end subroutine  


