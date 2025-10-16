subroutine readblockform(eof,core,blknum,formatted)
    !this routine needs to be refactored into subroutines 
    use variables
    use configs
    implicit none 
    integer,intent(inout) :: blknum
    logical :: eof , core ,check
    integer :: nread ,iostat ,checkint
    integer :: max_iter = 2**30
    character*9 :: dummy 
    character*9 :: radIndicator = 'RADIATIVE'

    logical :: formatted 

    character*3 :: char3 
    real*8 :: thisground
    integer :: cf1,cf2
    INTEGER :: COREINT 
    integer, allocatable :: amICore(:),configMarker(:)

    integer :: d1,d2

    !initializations.
    LVMAP = 0 
    numberContinuum= 0
    continuumIdex = 0
    continuumIdexprev=-1
    emptyChar = '          '

    COREINT = 0 
    checkint = 0
    if (core) coreint = 1 
    
    !check for end of file.
    if (formatted) then 
        read(1,*,iostat = iostat)
    else 
        read(1,iostat = iostat)
    end if  

    if (IS_IOSTAT_END(iostat)) then 
        eof = .true. 
        print*,' End of file detected, exiting this file. Iostat=',iostat
        return 
    end if 

    !go back
    backspace(1)


    blknum = blknum + 1
    print*,'Block ',blknum,'core = ',core
    princN = 0 
    orbL   = 0 

    !using iostat to catch an exception with doing too many reads
    !it would have been more stable had NRB told the oic file how many 
    !orbitals are present.
    if (formatted) then 
        read(1,'(A3,12X,I2,6X,I2,4X,100(I3,I2))',iostat=iostat) char3,nzed &
                            ,nelec,(princN(ii),orbL(ii),ii=1,totalshelldim)
        read(char3,'(I3)') nread
    else 
        read(1,iostat = iostat) nread, nzed ,nelec,(princN(ii),orbL(ii),ii=1,totalshelldim)
    end if 

    !it is now time to read the configurations. 
    print*,'I AM READING',nread,iostat
    allocate(amICore(nread),configMarker(nread))
    numblocks = numblocks + 1

    ! do ii = 1,nread
    !     read(1,'(I5)') configMarker(ii)
    ! end do 
    ! do  ii =1,nread 
    !     backspace(1)
    ! end do 
    ! call decode_eissner(nread)

    call decode_eissner(nread,formatted)
    configMarker(1:nread) = NII(1:nread) !transfer the config maerks.
    ! if (formatted) then 
    !     do ii = 1,nread 
    !         read(1,*)
    !     end do 
    ! else 
    !     do ii = 1,nread 
    !         read(1)
    !     end do 
    ! end if 

    amICore = 0 
    do ii = nread,1,-1 
        if (configMarker(ii) .gt. 0) then 
            amICore(ii) = 1 
        else
            exit 
        end if 
    end do 

    if (formatted) then 
        read(1,*)
        read(1,*)
    else 
        read(1)
    end if 

    AAARRAY =0.0d0
    numresfound = 0 
112  FORMAT(5I5,5X,1PE15.5,2(0PF15.6))

    if (formatted) then 
        do ii = 1,max_iter 
            read(1,112,iostat=iostat) cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 
            if (cf1.eq.0)  exit
            checkint = 0
            check = .false.
            check = XNOR( core , amICore(cf1).gt.0)
            if(check) checkint = 1
            if (check) then
                numresfound = numresfound + 1
                if (numresfound .gt. numres) then 
                    !put a dynamic reallocation here
                    stop 'dimensions exceed - numres'
                end if 
                AAARRAY (numresfound)    = aa 
                LV1ARRAY(numresfound)    = LV1 
                LV2ARRAY(numresfound)    = LV2 
                W_RES_STATE(numresfound) = w 
                E_RES_STATE(numresfound) = ediff+e1
            end if 
        !print'(6I5,5X,1PE15.5,2(0PF15.6),3I5)',numresfound,cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 , coreint,amICore(cf1),checkint
        end do 
    else !we are in an unformatted file. 
        do ii = 1,max_iter 
            read(1,iostat=iostat) cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 
            if (cf1.eq.0)  exit
            checkint = 0
            check = .false.
            check = XNOR( core , amICore(cf1).gt.0)
            if(check) checkint = 1
            if (check) then
                numresfound = numresfound + 1
                if (numresfound .gt. numres) then 
                    !put a dynamic reallocation here
                    stop 'dimensions exceed - numres'
                end if 
                AAARRAY (numresfound)    = aa 
                LV1ARRAY(numresfound)    = LV1 
                LV2ARRAY(numresfound)    = LV2 
                W_RES_STATE(numresfound) = w 
                E_RES_STATE(numresfound) = ediff+e1
            end if 
        !print'(6I5,5X,1PE15.5,2(0PF15.6),3I5)',numresfound,cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 , coreint,amICore(cf1),checkint
        end do 
    end if 


    print*,'I have found ',numresfound, ' resonances.'


123  FORMAT(5X,6I5,F15.6,i10) 


    !read stuff
    if (formatted) then 
        read(1,'(A10,I5,45X,F15.6)',iostat=iostat) dummy,nlevels,thisground
    else 
        read(1,iostat=iostat) nlevels,thisground
    end if 

    PRINT*,'nlevels,iostat=',NLEVELS,iostat,thisground
    if (iostat.lt.0) stop
    !skip the next line
    if (formatted) then 
        read(1,*)
    else
        read(1)
    end if 
    if (formatted) then 
        do ii = 1,nlevels 
            contIndexChar = emptyChar
            kk = 0
            read(1,123) lv,kk,kk,kk,kk,kk,ediff,kk
            E_RES_STATE(lv) = ediff + thisground
            if (kk.ne.0) then 
                continuumIdex = kk 
                if (continuumIdex .ne. continuumIdexprev) then 
                    numberContinuum = numberContinuum + 1
                end if 
                LVMAP(LV) = numberContinuum
                if (continuumIdex .eq. 1) then 
                    groundOfCont = ediff 
                end if 
                energyNstates(continuumIdex) = ediff 
                continuumIdexprev = continuumIdex
            end if 
        end do 
    else   !unformatted
        do ii = 1,nlevels 
            contIndexChar = emptyChar
            kk = 0
            read(1) lv,kk,kk,kk,kk,kk,ediff,kk
            E_RES_STATE(lv) = ediff + thisground
            if (kk.ne.0) then 
                continuumIdex = kk 
                if (continuumIdex .ne. continuumIdexprev) then 
                    numberContinuum = numberContinuum + 1
                end if 
                LVMAP(LV) = numberContinuum
                if (continuumIdex .eq. 1) then 
                    groundOfCont = ediff 
                end if 
                energyNstates(continuumIdex) = ediff 
                continuumIdexprev = continuumIdex
            end if 
        end do 
    end if 

    write(25,*) 'Ground of this cont is',groundOfCont
    energyNstates = energyNstates - groundOfCont 

    allocate( AARATE_SORTED(nlevels,numberContinuum ))
    allocate( E_RES_SORTED (nlevels) )
    allocate( W_SORTED     (nlevels) )

    AARATE_SORTED = 0.0d0 
    E_RES_SORTED = 0.0D0 
    W_SORTED = 0.0D0

    !process the resonances into N+1 -> N rates 
    !keep track of the energies and statistical weights
    do ii = 1,numresfound
        LV1 = LV1ARRAY(II)
        LV2 = LV2ARRAY(II)
        AARATE_SORTED(LV1,LVMAP(LV2)) = AARATE_SORTED(LV1,LVMAP(LV2))  + abs(AAARRAY(ii))
        W_SORTED     (LV1) = W_RES_STATE(II)
    end do  
    
    !tranfer these - order of these is fine 
    E_RES_SORTED (1:NLEVELS) = E_RES_STATE(1:nlevels)

    !Calculate branching ratios. 
    allocate(branching_ratio(nlevels,numberContinuum))
    branching_ratio = AARATE_SORTED 
    do jj = 1,nlevels 
        suma = sum( branching_ratio( jj , : ) )
        if(suma.gt.0)branching_ratio(jj,:)=branching_ratio(jj,:)/suma
    end do 

    !do jj = 1,nlevels 
    !    do ii = 1,numberContinuum 
    !        if ( AARATE_SORTED(jj,ii) .gt. 1e-20) then 
    !            print'(2I5,2F14.7,2ES11.2)', jj,ii,E_RES_SORTED(jj), energyNstates(ii),AARATE_SORTED(jj,ii),branching_ratio(jj,ii)
    !        end if 
    !    end do 
    !end do 

    !Calculate upsilons - the whole reason I'm here.
    if (.not. allocated(upsilon)) then 
        allocate( upsilon(ntemps , nlevels , nlevels ) )
        upsilon = 0.0d0 
    end if
    call resonantUpsilon

    !free stuff we don't need anymore - I should run this through 
    !valgrind and look for any leaks. 
    deallocate( AARATE_SORTED)
    deallocate( E_RES_SORTED )
    deallocate( branching_ratio)
    deallocate( W_SORTED     )

    if (formatted) then 
        read(1,'(41X,A9)') dummy 
        print*,dummy
        cf1 = 1 
        cf2 = 1
        if (dummy .eq. radIndicator) then 
            READ(1,*)
            !skip radiative rates until we find something new
            do ii = 1,max_iter
                read(1,113,iostat=iostat)cf1,lv1,w,cf2,lv2, w, aa ,ediff ,e1 
                if (cf1.eq.0)  exit
            end do 
        end if 
        113  FORMAT(6I5,1PE15.5,2(0PF15.6))
    else !we ar unformatted
        read(1) d1,d2 
        !print*,dummy
        cf1 = 1 
        cf2 = 1
        if (d1 .eq.nzed .and. d2.eq.nelec) then 
            !skip radiative rates until we find something new
            do ii = 1,max_iter
                read(1,iostat=iostat)cf1,lv1,w,cf2,lv2, w, aa ,ediff ,e1 
                if (cf1.eq.0)  exit
            end do 
        end if 
    end if 


    print*,'finished skipping radiative'
    !STOP 

    numberContinuumSave = numberContinuum


    contains 
    
    function XNOR(L1,L2) 
        logical :: l1,l2,xnor 

        xnor = .false. 

        if (l1.and.l2) xnor = .True. 
        if ( (.not.l1) .and. (.not.l2) ) xnor = .True.
    end function


end subroutine readblockform

