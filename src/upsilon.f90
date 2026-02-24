subroutine resonantUpsilon( )
    use variables, only:E_RES_SORTED,AARATE_SORTED,&
    branching_ratio,nlevels,W_SORTED,&
    upsilon,energyFromInput,nmax
    use omp_lib
    use constants
    use tempgrid
    !add contribution from this resonance block d to the upsilons. 
!
!           Ups(i,f) --> Ups(i,f) +  1\kT * Σ_d exp(-E_df / kT) * C * B,
!        where,
!           C(i,d) = 0.5 * g_d * h * Auger(d,i),
!        and B is the resonant-excitation branching ratio,
!           B(d,f) = Auger(d,f)  / Σ_f' Auger(d,f'),
!        which is undamped in this implementation. 
!
    !this can absolutely be made better
    !this code is incredibly ugly 
    !i think the array accesses are probably quite slow 
    implicit none 
    real*8 :: a,b,w
    real*8 :: energydiff
    real*8,allocatable :: temps_ryd(:) 
    real*8,allocatable :: oneOverTArray(:) 
    real*8 :: oneOverT 
    real*8 :: tfac 
    real*8 :: strength 
    integer :: lower,upper,dd,kk

        
    temps_ryd = temps_kelvin * boltz_ryd
    oneOverTArray = 1./ temps_ryd 

    !The below reduction parallelism is not very memory efficient and 
    !requires a relatively new openmp version. Given that the large 
    !resonance arrays remain shared - it's probably fine. 

    !If the number of lower levels is large enough, it might be better
    !to parallise over that. 

    !The below loops are labelled so that internal calls to CYCLE are 
    !clear what they are doing. 

    !$omp parallel private(dd,lower,upper,a,b,w,strength,kk,energydiff,oneOverT,tfac) REDUCTION(+:upsilon)
    !$omp do schedule(static) !!!
!
    resonant_loop: do dd = 1, nlevels !number of lv numbers
      w = W_SORTED(dd)
!
      lower_loop: do lower = 1,nmax 
        a = AARATE_SORTED  (dd,lower) * w * h_ryd_on_2  
        if (a .le. 0.0d0 ) cycle lower_loop !skip if the dielectronic capture is negligible.
!
        upper_loop: do upper = lower+1,nmax
          energydiff = E_RES_SORTED(dd)-energyFromInput(upper)                                  
          b = branching_ratio(dd,upper)
          if ( (energydiff .gt. 0.0d0) .and. (b .gt. 0.0d0) ) then 
            strength = a * b 
!
            temp_loop: do kk = 1, ntemps 
              oneOverT = oneOverTArray(kk)
              tfac = exp(- energydiff * oneOverT )* oneOverT 
              upsilon(kk,lower,upper) = &
              upsilon(kk,lower,upper) + strength * tfac
            end do temp_loop
!
          end if
        end do upper_loop
!
      end do lower_loop
!
    end do resonant_loop
!
    !$OMP END DO
    !$omp end parallel
end subroutine