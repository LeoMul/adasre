module variables
    !todo - get rid of some of these variables 
    !and move them to local subroutine variables 
    implicit none
    !input variables
    integer :: numtot
    integer :: nmax
    integer :: initresdim 
    !Temperature Grid 
    integer, parameter :: ntemps = 13
    real*8             :: temps(ntemps)
    !fixed arrays for read in 
    integer             :: numresdefault =  1000000
    integer             :: numres 
    integer,allocatable :: LV1ARRAY(:)
    integer,allocatable :: LV2ARRAY(:)
    real*8 ,allocatable :: AAARRAY (:)
    real*8,parameter :: RYDBERGCM= 109737.316
    integer :: numblocks = 0
    !input stuff
    integer            :: ntar1 
    integer            :: nzed,nelec
    real*8, allocatable:: energyFromInput(:)
    integer,allocatable:: angSFromInput  (:)
    integer,allocatable:: angLFromInput  (:)
    integer,allocatable:: angPFromInput  (:)
    integer,allocatable:: angJFromInput  (:) !technically 2J
    integer,allocatable:: cfNumFromInput (:)
    real*8             :: groundFromInput 
!
    !Upsilon.
    real*8,allocatable  :: upsilon(:,:,:)
!
    !Some tracking integers - should instead be local variables maybe.
    integer :: continuumIdex,continuumIdexprev
    integer :: lv 
    integer :: nlevels
!
    !Arrays for storing the Auger rates in terms of the true bound 
    !states indices.
    !for each block, all'd and free'd each time.
    real*8, allocatable :: AARATE_SORTED(:,:)
    real*8, allocatable :: E_RES_SORTED(:)
    real*8, allocatable :: W_SORTED(:)
    integer,allocatable :: LVMAP(:)
    real*8,allocatable  :: branching_ratio(:,:)
    integer :: offset
    integer  :: numberContinuum,numberContinuumSave
    real*8 :: suma
    integer :: numresfound
    character*10 :: contIndexChar,emptyChar
!
    contains 
!
    subroutine initReadInArrays 
!       Initializes read in arrays.
        implicit none 
        allocate(LV1ARRAY(numres))
        allocate(LV2ARRAY(numres))
        allocate(AAARRAY (numres))
    end subroutine 

    subroutine deallocateReadInArrays
!       Cleans up read in arrays. 
        implicit none 
        deallocate(LV1ARRAY)
        deallocate(LV2ARRAY)
        deallocate(AAARRAY )
    end subroutine

    subroutine extendReadInArrays
!       Extends the temporary read in arrays by 50%.  
        implicit none 
        integer, allocatable :: lv1tmp(:)
        integer, allocatable :: lv2tmp(:)
        real*8,  allocatable :: aaatmp(:) 
        integer :: oldNumRes 
        integer :: stat1,stat2,stat3
!
        oldNumRes = numres 
        numres = numres + numres / 2 !extend the array by 50% 
!
        write(0 ,1) oldNumRes, numres
        write(26,1) oldNumRes, numres
        call flush(26)
!
        allocate(lv1tmp(numres),stat = stat1)
        if (stat1.gt.0) stop 'Error allocating lv1tmp for reallocation.'
!      
        allocate(lv2tmp(numres),stat = stat2)
        if (stat2.gt.0) stop 'Error allocating lv2tmp for reallocation.'
!
        allocate(aaatmp(numres),stat = stat3)
        if (stat3.gt.0) stop 'Error allocating aaatmp for reallocation.'
!
        lv1tmp(1:oldNumRes) = LV1ARRAY(:)
        lv2tmp(1:oldNumRes) = LV2ARRAY(:)
        aaatmp(1:oldNumRes) = AAARRAY (:)
!
        call move_alloc(lv1tmp,LV1ARRAY)
        call move_alloc(lv2tmp,LV2ARRAY)
        call move_alloc(aaatmp,AAARRAY )
!
        write(0 ,2)
        write(26,2)
        call flush(26)
1   FORMAT(5X,'Attempting to reallocate from ',I10,' --> ',I10,'.')
2   FORMAT(5X,'Success!')
    end subroutine 
end module variables