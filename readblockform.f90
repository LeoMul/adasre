subroutine readblockform(eof,core,blknum)
    use variables
    use configs
    implicit none 
    integer,intent(inout) :: blknum
    logical :: eof , core ,check
    integer :: nread ,iostat ,checkint
    integer :: max_iter = 2**30
    character*9 :: dummy 
    character*9 :: radIndicator = 'RADIATIVE'

    character*3 :: char3 
    real*8 :: thisground
    integer :: cf1,cf2
    INTEGER :: COREINT 
    integer, allocatable :: amICore(:),configMarker(:)

    !initializations.
    LVMAP = 0 
    numberContinuum= 0
    continuumIdex = 0
    continuumIdexprev=-1
    emptyChar = '          '

    COREINT = 0 
    checkint = 0
    if (core) coreint = 1 
        
    read(1,*,iostat = iostat)
    if (IS_IOSTAT_END(iostat)) then 
        eof = .true. 
        return 
    end if 
    backspace(1)
    blknum = blknum + 1
    print*,'Block ',blknum,'core = ',core
    princN = 0 
    orbL   = 0 
    read(1,'(A3,12X,I2,6X,I2,4X,100(I3,I2))',iostat=iostat) char3,nzed &
                        ,nelec,(princN(ii),orbL(ii),ii=1,totalshelldim)

    read(char3,'(I3)') nread


    print*,'I AM READING',nread,iostat
    allocate(amICore(nread),configMarker(nread))
    numblocks = numblocks + 1

    do ii = 1,nread
        read(1,'(I5)') configMarker(ii)
    end do 
    do  ii =1,nread 
        backspace(1)
    end do 

    call decode_eissner(nread)

    amICore = 0 
    do ii = nread,1,-1 
        if (configMarker(ii) .gt. 0) then 
            amICore(ii) = 1 
        else
            exit 
        end if 
    end do 
    !print*,'hello am i the core ',core,amICore


    read(1,*)
    read(1,*)

    AAARRAY =0.0d0
    numresfound = 0 
112  FORMAT(5I5,5X,1PE15.5,2(0PF15.6))
    do ii = 1,max_iter 
        read(1,112,iostat=iostat) cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 
        if (cf1.eq.0)  exit
        checkint = 0
        check = .false.
        check = XNOR( core , amICore(cf1).gt.0)
        if(check) checkint = 1
        !if ( XNOR( core , amICore(cf1).gt.0)   ) then 
        if (check) then
        !if (.not. ( (.not. core) .and. configMarker(cf1).gt.0 ) ) then

        numresfound = numresfound + 1

        if (numresfound .gt. numres) stop 'dimensions exceed - numres'

        AAARRAY (numresfound)    = aa 
        LV1ARRAY(numresfound)    = LV1 
        LV2ARRAY(numresfound)    = LV2 
        W_RES_STATE(numresfound) = w 
        E_RES_STATE(numresfound) = ediff+e1

        !AARATE_CONT(lv1,lv2) = aa 


         
            !print*,LV1ARRAY(26),LV2ARRAY(26)
    end if 
    !print'(6I5,5X,1PE15.5,2(0PF15.6),3I5)',numresfound,cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 , coreint,amICore(cf1),checkint
        !if (lv1 .eq. 375) stop 'stopping here'
    end do 
    print*,'I have found ',numresfound, ' resonances.'
    !print*,cf


123  FORMAT(5X,6I5,F15.6,i10) 

    !read(1,*)qewwqe
    !print*,qewwqe
    read(1,'(A10,I5,45X,F15.6)',iostat=iostat) dummy,nlevels,thisground
    PRINT*,'nlevels,iostat=',NLEVELS,iostat,thisground
    if (iostat.lt.0) stop
    read(1,*)

    do ii = 1,nlevels 
        contIndexChar = emptyChar
        !do stuff with this 
        kk = 0
        read(1,123) lv,kk,kk,kk,kk,kk,ediff,kk
        !print'(F15.6,i10)',ediff,kk
        !print*,contIndexChar,contIndexChar.eq.emptyChar
        !we are in an N+1 correlation state.
        E_RES_STATE(lv) = ediff + thisground
        if( kk .eq. 0) then   

        else !we are in an continuum state.

            !read(contIndexChar,*) continuumIdex 
            continuumIdex = kk 
            if (continuumIdex .ne. continuumIdexprev) then 
                numberContinuum = numberContinuum + 1
            end if 
            LVMAP(LV) = numberContinuum
            !print*,lv,continuumIdex 
            if (continuumIdex .eq. 1) then 
                groundOfCont = ediff 
            end if 
            energyNstates(continuumIdex) = ediff 
            continuumIdexprev = continuumIdex
        end if 


    end do 
    write(25,*) ' ground of cont is',groundOfCont
    write(25,*) E_RES_STATE(124)
    energyNstates = energyNstates - groundOfCont 
    !E_RES_STATE = E_RES_STATE - groundOfCont
    !print*,'my shape is',shape(W_SORTED),numblocks

    !if (allocated(W_SORTED)) deallocate(W_SORTED)

    allocate( AARATE_SORTED(nlevels,numberContinuum ))
    allocate( E_RES_SORTED (nlevels) )
    allocate( W_SORTED     (nlevels) )
    AARATE_SORTED = 0.0d0 
    E_RES_SORTED = 0.0D0 
    W_SORTED = 0.0D0

    do ii = 1,numresfound

        LV1 = LV1ARRAY(II)
        LV2 = LV2ARRAY(II)
        !print*,'transferring',lv1,lv2,ii,numresfound,LV1ARRAY(26)
        AARATE_SORTED(LV1,LVMAP(LV2)) = AARATE_SORTED(LV1,LVMAP(LV2))  + abs(AAARRAY(ii))
        W_SORTED     (LV1) = W_RES_STATE(II)
        !just ii here - we corrected the ordering above

    end do  
    
    E_RES_SORTED (1:NLEVELS) = E_RES_STATE(1:nlevels)



    !if (numberContinuumSave .ne. numberContinuum) print*,'hello continuum error'

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

    if (.not. allocated(upsilon)) then 
        allocate( upsilon(ntemps , nlevels , nlevels ) )
        upsilon = 0.0d0 
    end if 

    call resonantUpsilon

    deallocate( AARATE_SORTED)
    deallocate( E_RES_SORTED )
    deallocate( branching_ratio)
    deallocate( W_SORTED     )

    read(1,'(41X,A9)') dummy 
    print*,dummy

113  FORMAT(6I5,1PE15.5,2(0PF15.6))
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

