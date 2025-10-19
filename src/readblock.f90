subroutine readblock(eof,core,blknum,formatted,firstread)
    !lpm 16.10.25: 
    !This reads an oic file, formatted or unformatted.
    !Heavy reads are coded twice - once for form and once for unform.
    !This has better performance than checking the formatted logic every
    !time a record is read.
    !This code is i/o limited - optimizations such as this matter?
    !It however comes at the cost of some code needs to be edited in
    !more than one place. I personally can live with this, but the user
    !may find it useful to check any code they edit. 

    !this routine needs to be refactored into subroutines 

    !Current todo's here:
    !1. some refactoring of the logic, i.e when do we include the core?
    !2. possible writes to disk if memory becomes a problem...
    !3. The oic file (might?) be arranged in order of autoionizing LV,
    !   which could allow for on-the-fly calculation of branching ratio
    !   except we need to know in advance what continuaa correspond to 
    !   each lower lv - which we don't in general due to the ordering of
    !   the blocks... 
    use variables
    use configs
    implicit none 
    
    !input variables. 
    logical,intent(inout) :: eof
    logical,intent(in   ) :: formatted   
    integer,intent(inout) :: blknum
    logical,intent(inout) :: core
    logical,intent(inout) :: firstread

    !Flags for reading 
    logical :: check
    integer :: nread ,iostat ,checkint,coreint
    integer :: cf1,cf2
    integer :: d1,d2,twoj
    integer, allocatable :: amICore(:),configMarker(:)

    !fixed iter - needs to be refactored
    integer :: max_iter = 2**30

    !temporary character variables 
    character*9 :: dummy 
    character*9 :: radIndicator = 'RADIATIVE'
    character*3 :: char3 
    
    !iters 
    integer :: ii ,jj, kk 

    !ground of this n-l (or core) block
    real*8 :: thisground
    
    real*8 :: t1,t2 
    !dummy reads
    integer :: lv1,w,lv2
    real*8  :: aa,ediff,e1
    real*8 :: groundOfCont

    !initializations.
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
    
    !check we are not running off the end 
!110 FORMAT('("End of file detected, exiting this file. Iostat=",I5)')
110 FORMAT ("End of file detected, exiting this file. Iostat=",I5)

    if (IS_IOSTAT_END(iostat)) then 
        eof = .true. 
        write(90,110) iostat
        return 
    end if 
!    write(99, '("Resonance read time in block",I5,": ",F20.2," sec.")')&
!                blknum, t2-t1
    !go back - it's rewind time 
    backspace(1)

    !Keep track of the number of blocks read for my records. 
    blknum = blknum + 1
    write(90,*)'Block ',blknum,'core = ',core

    !Initialize 
    princN = 0 
    orbL   = 0 

    !using iostat to catch an exception with doing too many reads
    !it would have been more stable had NRB told the oic file how many 
    !orbitals are present - but this appears to work, at least on gnu, 
    !arm64. It also appears to work on gnu-fortran, perlmutter.
    !Time - and testing - will reveal if this needs to be coded better. 
    if (formatted) then 
       
        read(1,'(A3,12X,I2,6X,I2,4X,100(I3,I2))',iostat=iostat) & 
        char3,nzed ,nelec,(princN(ii),orbL(ii),ii=1,totalshelldim)
        read(char3,'(I3)') nread
    else 
        read(1,iostat = iostat) nread, nzed ,nelec, &
                        (princN(ii),orbL(ii),ii=1,totalshelldim)
    end if 
    !it is now time to read the configurations. 
    allocate(amICore(nread),configMarker(nread))
    numblocks = numblocks + 1
    !Get the configs.
    call decode_eissner(nread,formatted)
    !transfer the config markers.
    configMarker(1:nread) = NII(1:nread) 
    !Tell the code which CF's are core (non-Rydberg)
    amICore = 0 
    do ii = nread,1,-1 
        !The CSF labels are arranged in the oic file in order of:
        ! 1. Continuum     (NII>0)
        ! 2. Rydberg       (NII<0)
        ! 3. Correlation   (NII>0)
        !So, loop backwards to get the correlation configurations.
        if (configMarker(ii) .gt. 0) then 
            amICore(ii) = 1 
        else
            exit 
        end if 
    end do 
!For reasons I do not understand - there is an extra blank space
!in the formatted file.
!Although this could be wrong.
    if (formatted) then 
        read(1,*)
        read(1,*)
    else 
        read(1)
    end if 

    AAARRAY =0.0d0
    numresfound = 0 
112  FORMAT(5I5,5X,1PE15.5,2(0PF15.6))

    call cpu_time(t1)

    if (formatted) then 
        do ii = 1,max_iter 
            read(1,112,iostat=iostat) cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 
            if (cf1.eq.0)  exit
            checkint = 0
            check = .false.
            check = XNOR( core , amICore(cf1).gt.0)
            if(check) checkint = 1
            if (check.or.firstread) then
                numresfound = numresfound + 1
                if (numresfound .gt. numres) then 
                    call extendReadInArrays
                    !put a dynamic reallocation here
                    !stop 'dimensions exceed - numres'
                end if 
                AAARRAY    (numresfound) = aa 
                LV1ARRAY   (numresfound) = LV1 
                LV2ARRAY   (numresfound) = LV2 
            end if 
        !print'(6I5,5X,1PE15.5,2(0PF15.6),3I5)',numresfound,cf1,lv1,w, &
        !cf2,lv2 , aa ,ediff ,e1 , coreint,amICore(cf1),checkint
        end do 
    else !we are in an unformatted file. 
        do ii = 1,max_iter 
            read(1,iostat=iostat) cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 
            if (cf1.eq.0)  exit
            checkint = 0
            check = .false.
            check = XNOR( core , amICore(cf1).gt.0)
            !print'(6I5,5X,1PE15.5,2(0PF15.6),3I5)',numresfound,cf1,lv1&
            !,w,cf2,lv2 , aa ,ediff ,e1 , coreint,amICore(cf1),checkint
            if(check) checkint = 1
            if (check.or.firstread) then
                numresfound = numresfound + 1
                if (numresfound .gt. numres) then 
                    call extendReadInArrays
                    !put a dynamic reallocation here
                    !stop 'dimensions exceed - numres'
                end if 
                AAARRAY (numresfound)    = aa 
                LV1ARRAY(numresfound)    = LV1 
                LV2ARRAY(numresfound)    = LV2 
            end if 
        !print'(6I5,5X,1PE15.5,2(0PF15.6),3I5)',numresfound,cf1,lv1,w,&
        !cf2,lv2 , aa ,ediff ,e1 , coreint,amICore(cf1),checkint
        end do 
    end if 

    call cpu_time(t2)

    write(99, '(" Resonance read time in block",I5,": ",F20.2," sec.")')&
                blknum, t2-t1
    print '(" Resonance read time in block",I5,": ",F20.2," sec.")',&
                blknum, t2-t1
    !we only need the core from one block, so set it to false if we 
    !havent already.
    
    if(core) then 
        core = .false. 
        write(90,*) 'Setting core      to false. I have counted the &
&       core in this block.'
    end if 
    if(firstread) then 
        firstread = .false. 
        write(90,*) 'Setting firstread to false. I have counted the &
&       core in this block.'
    end if 


    write(90,*)'I have found ',numresfound, ' resonances.'


123  FORMAT(5X,6I5,F15.6,i10) 


    !read stuff
    if (formatted) then 
        read(1,'(A10,I5,45X,F15.6)',iostat=iostat) &
        dummy,nlevels,thisground
    else 
        read(1,iostat=iostat) nlevels,thisground
    end if 

    write(90,*)'nlevels,iostat=',NLEVELS,iostat,thisground
    if (iostat.lt.0) stop
    !skip the next line
    if (formatted) then 
        read(1,*)
    else
        read(1)
    end if 

    allocate( lvmap(nlevels) )
    allocate( E_RES_SORTED (nlevels) )
    allocate( W_SORTED     (nlevels) )
    E_RES_SORTED = 0.0D0 
    W_SORTED = 0.0D0
    LVMAP = 0 

    call cpu_time(t1)

    if (formatted) then 
        do ii = 1,nlevels 
            contIndexChar = emptyChar
            kk = 0
            read(1,123) lv,kk,kk,kk,twoj,kk,ediff,kk
            E_RES_SORTED(lv) = ediff + thisground
            W_SORTED(lv) = twoj + 1
            if (kk.ne.0) then 
                continuumIdex = kk 
                if (continuumIdex .ne. continuumIdexprev) then 
                    numberContinuum = numberContinuum + 1
                end if 
                LVMAP(LV) = numberContinuum
                if (continuumIdex .eq. 1) then 
                    groundOfCont = ediff 
                end if 
                continuumIdexprev = continuumIdex
            end if 
        end do 
    else   !unformatted
        do ii = 1,nlevels 
            contIndexChar = emptyChar
            kk = 0
            read(1) lv,kk,kk,kk,twoj,kk,ediff,kk
            E_RES_SORTED(lv) = ediff + thisground
            W_SORTED(lv) = twoj + 1
            if (kk.ne.0) then 
                continuumIdex = kk 
                if (continuumIdex .ne. continuumIdexprev) then 
                    numberContinuum = numberContinuum + 1
                end if 
                LVMAP(LV) = numberContinuum
                if (continuumIdex .eq. 1) then 
                    groundOfCont = ediff 
                end if 
                continuumIdexprev = continuumIdex
            end if 
        end do 
    end if 

    call cpu_time(t2)
    write(99, '("Level read time in block",I5,": ",F20.2," sec.")' ) & 
    blknum, t2-t1
    print '(" Level read time in block",I5,": ",F20.2," sec.")',  & 
    blknum, t2-t1
    write(25,*) 'Ground of this cont is',groundOfCont

    allocate( AARATE_SORTED(nlevels,numberContinuum ))


    AARATE_SORTED = 0.0d0 


    !process the resonances into N+1 -> N rates 
    !keep track of the energies and statistical weights
    call cpu_time(t1)
    do ii = 1,numresfound
        LV1 = LV1ARRAY(II)
        LV2 = LV2ARRAY(II)
        AARATE_SORTED(LV1,LVMAP(LV2)) = AARATE_SORTED(LV1,LVMAP(LV2))  &
        + abs(AAARRAY(ii))
    end do  
    call cpu_time(t2)
    write(99, '("Resonance transfer time in block",I5,": ",F20.2," &
                                             &sec.")' ) blknum, t2-t1
    write(26, '("Resonance transfer time in block",I5,": ",F20.2," &
                                             &sec.")' ) blknum, t2-t1
    print'(" Resonance transfer time in block",I5,": ",F20.2," &
                                             &sec.")', blknum, t2-t1                                

    !tranfer these - order of these is fine 
    !E_RES_SORTED (1:NLEVELS) = E_RES_STATE(1:nlevels)

    !Calculate branching ratios. 
    call cpu_time(t1)
    allocate(branching_ratio(nlevels,numberContinuum))
    branching_ratio = AARATE_SORTED 
    do jj = 1,nlevels 
        suma = sum( branching_ratio( jj , : ) )
        if(suma.gt.0)branching_ratio(jj,:)=branching_ratio(jj,:)/suma
    end do 
    call cpu_time(t2)
    write(99, '("Branching ratio calc time in block",I5,": ",F20.2," &
                                           &sec.")' ) blknum, t2-t1
    print'(" Branching ratio calc time in block",I5,": ",F20.2," &
                                           &sec.")', blknum, t2-t1
    !Calculate upsilons - the whole reason I'm here.
    call cpu_time(t1)
    if (.not. allocated(upsilon)) then 
        allocate( upsilon(ntemps , nmax , nmax ) )
        upsilon = 0.0d0 
        write(26,*) 'allocated upsilon',ntemps , nmax , & 
        nmax, ' = ', ntemps*nmax*nmax
        call flush(26)
    end if
    call resonantUpsilon
    call cpu_time(t2)
    write(99, '("Upsilon calc time in block",I5,": ",F20.2," sec.")' ) &
                blknum, t2-t1
    print '(" Upsilon calc time in block",I5,": ",F20.2," sec.")', &
    & blknum, t2-t1

    !free stuff we don't need anymore - I should run this through 
    !valgrind and look for any leaks. 
    deallocate( AARATE_SORTED)
    deallocate( E_RES_SORTED )
    deallocate( branching_ratio)
    deallocate( W_SORTED     )

    call cpu_time(t1)
    if (formatted) then 
        read(1,'(41X,A9)') dummy 
        write(90,*)dummy
        cf1 = 1 
        cf2 = 1
        if (dummy .eq. radIndicator) then 
            READ(1,*)
            !skip radiative rates until we find something new
            do ii = 1,max_iter
                read(1,113,iostat=iostat)cf1,lv1,w,cf2,lv2,w,aa,ediff,e1 
                if (cf1.eq.0)  exit
            end do 
        end if 
        write(90,*)'finished skipping radiative'
        113  FORMAT(6I5,1PE15.5,2(0PF15.6))
    else !we ar unformatted
        read(1) d1,d2 
        !write(90,*)dummy
        cf1 = 1 
        cf2 = 1
        if (d1 .eq.nzed .and. d2.eq.nelec) then 
            !skip radiative rates until we find something new
            do ii = 1,max_iter
                read(1,iostat=iostat)cf1,lv1,w,cf2,lv2,w,aa,ediff,e1 
                if (cf1.eq.0)  exit
            end do 
        end if 
        write(90,*)'finished skipping radiative'
    end if 
    call cpu_time(t2)
    write(99, '("Radiative skip time in block",I5,": ",F20.2," sec.")')&
            blknum, t2-t1

    deallocate(lvmap)


    numberContinuumSave = numberContinuum
    !clean up after yourself.
    deallocate(amICore,configMarker)

    contains 
    
    function XNOR(L1,L2) 
        !xnor logical gate - might need to remove - refactor needed
        !where this code is called.
        logical :: l1,l2,xnor 

        xnor = .false. 

        if (l1.and.l2) xnor = .True. 
        if ( (.not.l1) .and. (.not.l2) ) xnor = .True.
    end function


end subroutine readblock

