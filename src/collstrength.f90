subroutine collstrength  
    !Calculates the resonant values of OMEGA.
!
    !For consistency with the distorted wave output of AUTOSTRUCTURE,
    !these are put onto an energy grid where the threshold is at zero.
    !There is therefore a single grid for all transitions, in some 
    !sense. 
! 
    !This is in constrast to the output of RMATRX. While there is a
    !single grid here also - the grid of stgf is relative to the ground
    !for all transitions. I.e the threshold is at E_f for all transitions.
!
    use variables
    use constants
    use kinds
    implicit none
! 
    integer(levelIter) :: myupper, mylower  
    integer(levelIter) :: ii,jj 
    real   (floatKind) :: energydiff 
!
    do ii = 1, collstreng 
      myupper = collstrengupper(ii)
      mylower = collstrenglower(ii)
      do jj = 1, nlevels 
        call lorentz 
        collstengthData(ii,:) = collstengthData(ii,:) + lorentzarray(:) 
      end do 
    end do 
!
    contains  

    subroutine     lorentz
        integer(largeIter) :: ii  
        real(floatKind)    :: width,width2  
        real(floatKind)    :: coefficient 
!       
        energydiff = E_RES_SORTED(jj) - energyFromInput(myUpper)
!
        width      = half * hbar_ryd * aasums(jj) 
        width2     = width* width 
!
        coefficient = h_ryd_on_2 * W_SORTED(jj) * AARATE_SORTED(jj,mylower) * branching_ratio(jj,myupper)
        coefficient = coefficient * overpi * width
        do ii = 1, collstrengnpoints 
            lorentzarray(ii) =  coefficient / ( (energyGrid(ii) - energydiff)**2 + width2 )
        end do 
        !print '(3ES10.3)',width,aasums(jj),energydiff
        !print*,lorentzarray

    end subroutine lorentz
!
end subroutine collstrength