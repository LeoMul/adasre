module variables
    implicit none
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

    !fixed dimensions, to be cleaned up 
    integer, parameter :: NUM_N1 = 1000 
    integer, parameter :: NUM_N  = 1000 
    integer, parameter :: NUM_C  = 20000 
    integer, parameter :: numres = 2000000 
!    integer, parameter :: nlevels = 210 
    integer :: nlevels
    integer, parameter :: ntemps = 13
    real*8             :: temps(ntemps)

    integer :: jj 
    integer :: ii, continuumIdex,continuumIdexprev
    integer :: lv 

    !arrays for calculations 
    real*8, allocatable :: AARATE_SORTED(:,:)
    real*8, allocatable :: E_RES_SORTED(:)
    real*8, allocatable :: W_SORTED(:)

    integer :: mapCtoN(NUM_C,NUM_N)
    integer :: LVMAP(NUM_C)
    real*8  :: AARATE_CONT(NUM_C,NUM_N)
    real*8,allocatable  :: branching_ratio(:,:)

    real*8,allocatable  :: upsilon(:,:,:)


    character*4 :: char4
    integer :: offset
    real*8  :: energyNstates(NUM_N)
    integer  :: numberContinuum,numberContinuumSave

    !fixed arrays for read in 
    integer :: LV1ARRAY(numres)
    integer :: LV2ARRAY(numres)
    real*8  :: AAARRAY (numres)
    real*8 :: E_RES_STATE(numres)
    real*8 :: W_RES_STATE(numres)
    real*8 :: suma




    integer :: numresfound
    integer :: cf,lv1,w,lv2
    real*8  :: aa,ediff,e1
    integer :: kk 
    character*10 :: contIndexChar,emptyChar

    real*8 :: groundOfCont



end module variables