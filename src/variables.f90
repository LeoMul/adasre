module variables
    implicit none

    !fixed arrays for read in 
    integer :: numres = 1000000 
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

    integer :: nlevels
    integer, parameter :: ntemps = 13
    real*8             :: temps(ntemps)

    integer :: continuumIdex,continuumIdexprev
    integer :: lv 

    !arrays for calculations 
    
    !for each block, all'd and free'd each time.
    real*8, allocatable :: AARATE_SORTED(:,:)
    real*8, allocatable :: E_RES_SORTED(:)
    real*8, allocatable :: W_SORTED(:)
    integer,allocatable :: LVMAP(:)
    real*8,allocatable  :: branching_ratio(:,:)
    !used through all of runtime.
    real*8,allocatable  :: upsilon(:,:,:)


    integer :: offset
    integer  :: numberContinuum,numberContinuumSave


    real*8 :: suma




    integer :: numresfound
    integer :: cf,lv1,w,lv2
    real*8  :: aa,ediff,e1
    integer :: kk 
    character*10 :: contIndexChar,emptyChar

    real*8 :: groundOfCont

    contains 

    subroutine initReadInArrays 
        implicit none 

        allocate(LV1ARRAY(numres))
        allocate(LV2ARRAY(numres))
        allocate(AAARRAY (numres))

    end subroutine 

    subroutine deallocateReadInArrays 
        implicit none 
        deallocate(LV1ARRAY)
        deallocate(LV2ARRAY)
        deallocate(AAARRAY )
    end subroutine

    subroutine extendReadInArrays 
        implicit none 
        integer, allocatable :: lv1tmp(:)
        integer, allocatable :: lv2tmp(:)
        real*8,  allocatable :: aaatmp(:) 
        integer :: oldNumRes 
        oldNumRes = numres 
        numres = numres + numres / 2 !extend the array by 50% 
        write(0 ,*) 'Attempting to realc from ', oldNumRes,' to ',numres
        write(26,*) 'Attempting to realc from ', oldNumRes,' to ',numres
        call flush(26)
        allocate(lv1tmp(numres))
        allocate(lv2tmp(numres))
        allocate(aaatmp(numres))

        lv1tmp(1:oldNumRes) = LV1ARRAY(:)
        lv2tmp(1:oldNumRes) = LV2ARRAY(:)
        aaatmp(1:oldNumRes) = AAARRAY (:)

        call move_alloc(lv1tmp,LV1ARRAY)
        call move_alloc(lv2tmp,LV2ARRAY)
        call move_alloc(aaatmp,AAARRAY )

        write(0 ,*) 'Successfully reallocated from ', oldNumRes,' to ',& 
        numres
        write(26,*) 'Successfully reallocated from ', oldNumRes,' to ',&
        numres
        call flush(26)

    end subroutine 



end module variables