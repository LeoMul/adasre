subroutine readblock(eof,core,blknum,formatted,firstread,filename)
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
    use omp_lib
    use kinds
    implicit none 
    
    !input variables. 
    logical,intent(inout) :: eof
    logical,intent(in   ) :: formatted   
    integer,intent(inout) :: blknum
    logical,intent(inout) :: core
    logical,intent(inout) :: firstread
    character(len=*)      :: filename

    !Data to be read directluy from AS output:
    integer(readInt) :: nread 
    integer(readInt) :: cf1,cf2
    integer(readInt) :: lv1,w,lv2
    integer(readInt) :: ireadzero = 0
    !Flags for reading 
    logical :: check
    integer :: iostat ,checkint,coreint
    integer :: d1,d2,twoj
    integer, allocatable :: amICore(:),configMarker(:)

    !fixed iter - needs to be refactored


    !temporary character variables 
    character*9 :: dummy 
    character*9 :: radIndicator = 'RADIATIVE'
    character*3 :: char3 
    
    !iters 
    integer(resonantIter) :: itwo = 2 
    integer(resonantIter) :: ii ,jj
    integer(resonantIter) :: max_iter! = itwo**62 
!
!
    integer(readInt)     :: kk 
    !ground of this n-l (or core) block
    
    real*8 :: t1,t2 
    !dummy reads
    real*8  :: aa,ediff,e1,ar
    real*8 :: groundOfCont
    real*8 :: arsum, aasum

    !initializations.
    max_iter = itwo**62
    numberContinuum= 0
    continuumIdex = 0
    continuumIdexprev=-1
    emptyChar = '          '
    oiccontinuumground = 0.0d0
    
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

    if (IS_IOSTAT_END(iostat)) then 
        eof = .true. 
        write(90,110) iostat
        return 
    end if 
!    write(99, '("Resonance read time in block",I5,": ",F20.2," sec.")')&
!                blknum, t2-t1
    !go back - it's rewind time 
    backspace(1)
    write(99,999)
    print 999
    !Keep track of the number of blocks read for my records. 
    blknum = blknum + 1
    print 203, filename,blknum
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
!
    AAARRAY =0.0d0
    numresfound = 0 
112  FORMAT(5I5,5X,1PE15.5,2(0PF15.6))
!
    call cpu_time(t1)
!
    if (formatted) then 
        do ii = 1,max_iter 
            read(1,112,iostat=iostat) cf1,lv1,w,cf2,lv2 , aa ,ediff ,e1 
            if (cf1.eq.0) exit
            checkint = 0
            check = .false.
            check = XNOR( core , amICore(cf1).gt.0)
            if(check) checkint = 1
            if (check.or.firstread) then
                numresfound = numresfound + 1
                if (numresfound .gt. numres) then 
                    call extendReadInArrays
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
!
    oiccontinuumground = e1
!
    call cpu_time(t2)
!
!   Writeout 
    write(99,101) blknum, t2-t1
    print 101,    blknum, t2-t1
    write(99,201) numresfound
    print 201,    numresfound
!
    call flush(99)
!   Flush output as clusters are slow.
    !
    !we only need the core from one block, so set it to false if we 
    !havent already.
!
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
!
!
    !read stuff
    if (formatted) then 
        read(1,'(A10,I5,45X,F15.6)',iostat=iostat) &
        dummy,nlevels,thisground
    else 
        read(1,iostat=iostat) nlevels,thisground
    end if 
!
    !write(90,*)'nlevels,iostat=',NLEVELS,iostat,thisground
    write(99,202) nlevels 
    if (iostat.lt.0) stop
    !skip the next line
    if (formatted) then 
        read(1,*)
    else
        read(1)
    end if 
!
    allocate( lvmap(nlevels) )
    allocate( E_RES_SORTED (nlevels) )
    allocate( W_SORTED     (nlevels) )
    E_RES_SORTED = 0.0D0 
    W_SORTED = 0.0D0
    LVMAP = 0 
!
    call cpu_time(t1)
!
123 FORMAT(5X,6I5,F15.6,i10) 
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
!
    call cpu_time(t2)
    write(99,102) blknum, t2-t1
    print 102,    blknum, t2-t1
!
    allocate( AARATE_SORTED(nlevels,numberContinuum ))
    allocate (aasums(nlevels))
    aasums = 0.0d0 
    AARATE_SORTED = 0.0d0 
!
    !process the resonances into N+1 -> N rates 
    !keep track of the energies and statistical weights
    !call cpu_time(t1)
    t1 = omp_get_wtime()
    !$omp parallel shared(AARATE_SORTED,aasums) private(ii,lv1,lv2)
    !$omp do 
    do ii = 1,numresfound
        LV1 = LV1ARRAY(II)
        LV2 = LV2ARRAY(II)
        aa = abs(AAARRAY(ii))
        AARATE_SORTED(LV1,LVMAP(LV2)) = AARATE_SORTED(LV1,LVMAP(LV2)) &
        + aa
        aasums(lv1) = aasums(lv1) + aa
    end do  
    !$omp end do 
    !$omp end parallel
    t2 = omp_get_wtime()
    !call cpu_time(t2)
    write(99,103) blknum, t2-t1
    print 103,    blknum, t2-t1    
    call flush(99)                            
!
    !tranfer these - order of these is fine 
    !E_RES_SORTED (1:NLEVELS) = E_RES_STATE(1:nlevels)

    !Calculate branching ratios. 
    !call cpu_time(t1)
    t1 = omp_get_wtime()
    allocate(branching_ratio(nlevels,numberContinuum))
    branching_ratio = AARATE_SORTED 
    !$omp parallel shared(branching_ratio) private(suma,jj,ii)
    !$omp do 
    do jj = 1,nlevels 
        suma = aasums(jj)
        if(suma.gt.0) then 
            do ii  = 1, numberContinuum 
                branching_ratio(jj,ii)=branching_ratio(jj,ii)/suma
            end do 
        end if 
    end do 
    !$omp end do 
    !$omp end parallel
    !call cpu_time(t2)
    t2 = omp_get_wtime()
    write(99,104) blknum, t2-t1
    print 104,    blknum, t2-t1
    call flush(99)
    !Calculate upsilons - the whole reason I'm here.
    !call cpu_time(t1)
    t1 = omp_get_wtime()
    if (.not. allocated(upsilon)) then 
        allocate( upsilon(ntemps , nmax , nmax ) )
        upsilon = 0.0d0 
        write(26,*) 'allocated upsilon',ntemps , nmax , & 
        nmax, ' = ', ntemps*nmax*nmax
        call flush(26)
    end if
    call resonantUpsilon
    if (collstreng>0) call collstrength
    !call cpu_time(t2)
    t2 = omp_get_wtime()
    write(99,105) blknum, t2-t1
    print 105,    blknum, t2-t1


    call cpu_time(t1)
!
    !allocate(ARRATE_SORTED(nlevels,nlevels))
    allocate(drwidth(nlevels))
    drwidth = 0.0d0
!
    if (formatted) then 
        read(1,'(41X,A9)') dummy 
        write(90,*)dummy
        cf1 = 1 
        cf2 = 1
        if (dummy .eq. radIndicator) then 
            READ(1,*)
            !skip radiative rates until we find something new
            do ii = 1,max_iter
                read(1,113,iostat=iostat)cf1,lv1,w,cf2,lv2,w,ar,ediff,e1 
                if (cf1.eq.0)  exit
                drwidth(lv1) = drwidth(lv1) + abs(ar)
                numrr = numrr + 1 
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
                read(1,iostat=iostat)cf1,lv1,w,cf2,lv2,w,ar,ediff,e1 
                if (cf1.eq.0)  exit
                drwidth(lv1) = drwidth(lv1) + abs(ar)
                numrr = numrr + 1 
            end do 
        end if 
        write(90,*)'finished skipping radiative'
    end if 
!
    call cpu_time(t2)
    write(99, 106) blknum, t2-t1

    if (calcdr .ne. 0 ) then 
        !dr width 
        t1 = omp_get_wtime()
        !$omp parallel shared(drwidth) private(arsum,aasum,ii)
        !$omp do 
        do ii = 1, nlevels 
            !might need to be more careful here...
            aasum = sum( AARATE_SORTED(ii,:))
            if(aasum>0.0d0) then 
                drwidth(ii) = drwidth(ii)/(aasum+drwidth(ii))
            end if 
        end do 
        !$omp end do 
        !$omp end parallel

        t2 = omp_get_wtime()
        print*,'time in rad width ',t2-t1
        print*,'hello from lpm na,nr = ',numrr,numresfound
        t1 = omp_get_wtime()
        call coredr
        t2 = omp_get_wtime()
        print*,'time in core dr',t2-t1
    end if 
!

    write(99, 999)
    print 999
    call flush(99)
    deallocate(lvmap)
!
    numberContinuumSave = numberContinuum
    !clean up after yourself.
    deallocate(amICore,configMarker)
    deallocate( AARATE_SORTED)
    deallocate( aasums)
    deallocate( E_RES_SORTED )
    deallocate( branching_ratio)
    deallocate( W_SORTED     )
    !deallocate(ARRATE_SORTED)
    deallocate(drwidth)
!
    !write-out formats. 
!
101 FORMAT("  Resonance read time in block",I5,": ",F19.2," sec.")
102 FORMAT("  Level     read time in block",I5,": ",F19.2," sec.")
103 FORMAT("  Resonance tran time in block",I5,": ",F19.2," sec.")
104 FORMAT("  Br.Ratio  calc time in block",I5,": ",F19.2," sec.")
105 FORMAT("  Upsilon   calc time in block",I5,": ",F19.2," sec.")
106 FORMAT("  Radiative skip time in block",I5,": ",F19.2," sec.")
!
201 FORMAT("  I have found  ",I12," resonances.")
202 FORMAT("  Searching for ",I12," levels.")
203 FORMAT(2X,"File: ",A3,", Block number: ",I5)
!
999 FORMAT("----------------------------------------------------------&
&---")
110 FORMAT("End of file detected, exiting this file. Iostat=",I5)
!
    contains 
    
    function XNOR(L1,L2) 
        !xnor logical gate - might need to remove - refactor needed
        !where this code is called.
        logical :: l1,l2,xnor 

        xnor = .false. 

        if (l1.and.l2) xnor = .True. 
        if ( (.not.l1) .and. (.not.l2) ) xnor = .True.
    end function


    subroutine readRad 
        if (formatted) then 
            read(1,'(41X,A9)') dummy 
            write(90,*)dummy
            cf1 = 1 
            cf2 = 1
            if (dummy .eq. radIndicator) then 
                READ(1,*)
                !skip radiative rates until we find something new
                do ii = 1,max_iter
                    read(1,113,iostat=iostat)cf1,lv1,w,cf2,lv2,w,ar,ediff,e1 
                    if (cf1.eq.0)  exit
                    drwidth(lv1) = drwidth(lv1) + abs(ar)
                    numrr = numrr + 1 
                end do 
            end if 
            write(90,*)'finished reading radiative'
            113  FORMAT(6I5,1PE15.5,2(0PF15.6))
        else !we ar unformatted
            read(1) d1,d2 
            !write(90,*)dummy
            cf1 = 1 
            cf2 = 1
            if (d1 .eq.nzed .and. d2.eq.nelec) then 
                !skip radiative rates until we find something new
                do ii = 1,max_iter
                    read(1,iostat=iostat)cf1,lv1,w,cf2,lv2,w,ar,ediff,e1 
                    if (cf1.eq.0)  exit
                    drwidth(lv1) = drwidth(lv1) + abs(ar)
                    numrr = numrr + 1 
                end do 
            end if 
            write(90,*)'finished reading radiative'
        end if 
    end subroutine readRad 


    subroutine skipRad 
        if (formatted) then 
            read(1,'(41X,A9)') dummy 
            write(90,*)dummy
            cf1 = 1 
            cf2 = 1
            if (dummy .eq. radIndicator) then 
                READ(1,*)
                !skip radiative rates until we find something new
                do ii = 1,max_iter
                    read(1,113,iostat=iostat)cf1
                    if (cf1.eq.0)  exit
                end do 
            end if 
            write(90,*)'finished skipping radiative'
            113  FORMAT(I5)
        else !we ar unformatted
            read(1) d1,d2 
            !write(90,*)dummy
            cf1 = 1 
            cf2 = 1
            if (d1 .eq.nzed .and. d2.eq.nelec) then 
                !skip radiative rates until we find something new
                do ii = 1,max_iter
                    read(1,iostat=iostat)cf1
                    if (cf1.eq.0)  exit
                end do 
            end if 
            write(90,*)'finished skipping radiative'
        end if 
    end subroutine skipRad 

end subroutine readblock

