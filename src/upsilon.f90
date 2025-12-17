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
        real*8 :: contribution 
        real*8 :: tfac 
        real*8 :: strength 
        integer :: lower,upper,dd,kk

        
        temps_ryd = temps_kelvin * boltz_ryd
        oneOverTArray = 1./ temps_ryd 

        !$omp parallel shared(upsilon) private(dd,lower,upper,a,b,strength,kk,energydiff,oneOverT,tfac,contribution)
        !$omp do schedule(static) !!!
        do dd = 1, nlevels !number of lv numbers
!
            do lower = 1,nmax 
                a = AARATE_SORTED  (dd,lower)
!
                do upper = lower+1,nmax
!
                    energydiff = E_RES_SORTED(dd)-energyFromInput(upper)                                  
                    b = branching_ratio(dd,upper)
                    w = W_SORTED       (dd)
                    if (energydiff .gt. 0.0d0) then 
                     if ( (a .gt. 0.0d0) .and. (b .gt. 0.0d0)) then
                            strength = a * b * w * h_ryd_on_2 
                            do kk = 1, ntemps 
                                oneOverT = oneOverTArray(kk)
                                tfac = exp(- energydiff * oneOverT )
                                tfac = tfac * oneOverT 
                                contribution = strength * tfac 
                                upsilon(kk,lower,upper) = &
                                upsilon(kk,lower,upper) + contribution
                            end do
                        end if
                    end if 

                end do 
            end do 
        end do 
        !$OMP END DO
        !$omp end parallel
    end subroutine