subroutine collstrength  
    use variables, only: collstengthData,&
                         collstrenglower,&
                         collstrengupper,&
                         collstreng,&
                         numres,&
                         lorentzarray,&
                         E_RES_SORTED,&
                         energyFromInput,&
                         energyGrid,&
                         aasums,&
                         collstrengnpoints,&
                         W_SORTED,&
                         AARATE_SORTED,&
                         branching_ratio,&
                         nlevels
    implicit none 
    real*8 :: a,b,w
    real*8,parameter :: boltz_si = 1.38e-23
    real*8,parameter :: electrostat = 1.6e-19 
    real*8,parameter :: ryd_ev = 13.605703976
    real*8,parameter :: h_si  = 6.63e-34  
    real*8,parameter :: h_ryd = h_si / (electrostat * ryd_ev)
    real*8,parameter :: half= 0.5e0
    real*8,parameter :: h_ryd_on_2 = h_ryd * half
    real*8,parameter :: pi = 3.14159 
    real*8,parameter            :: overpi = 1./pi 
    integer :: myupper, mylower  
    integer :: ii,jj 
    real*8 :: energydiff 
!
    do ii = 1, collstreng 
!
        myupper = collstrengupper(ii)
        mylower = collstrenglower(ii)
        do jj = 1, nlevels 
            call lorentz 
            collstengthData(ii,:) = collstengthData(ii,:) + lorentzarray(:) 
        end do 
!
    end do 
!
    contains  

    subroutine     lorentz
        integer :: ii  
        real*8 :: width,width2  
        real*8 :: centre 
        real*8 :: coefficient 
        real*8 :: hbarryd = 0.5 * overpi * h_ryd
        energydiff = E_RES_SORTED(jj) - energyFromInput(mylower)
        width = 0.005d0 !0.5d0*hbarryd * aasums(jj)
        width2 = width*width 
        coefficient = h_ryd_on_2 * W_SORTED(jj) * AARATE_SORTED(jj,mylower) * branching_ratio(jj,myupper)
        coefficient = coefficient * overpi * width
        do ii = 1, collstrengnpoints 
            lorentzarray(ii) =  coefficient / ( (energyGrid(ii) - energydiff)**2 + width2 )
        end do 
        print '(3ES10.3)',width,aasums(jj),energydiff
        !print*,lorentzarray

    end subroutine lorentz
!
end subroutine collstrength