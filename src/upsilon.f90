    subroutine resonantUpsilon( )
        use variables, only:temps,E_RES_SORTED,AARATE_SORTED,&
        branching_ratio,nlevels,ntemps,numberContinuum,W_SORTED,&
        upsilon,energyFromInput,nmax
        !add contribution from this block to the upsilons. 
        !this can absolutely be made better
        !this code is incredibly ugly 
        !i think the array accesses are probably quite slow 
        implicit none 
        real*8 :: a,b,w

        real*8,parameter :: boltz_si = 1.38e-23
        real*8,parameter :: electrostat = 1.6e-19 
        real*8,parameter :: ryd_ev = 13.605703976
        real*8,parameter :: h_si  = 6.63e-34  
        real*8,parameter :: h_ryd = h_si / (electrostat* ryd_ev)
        real*8,parameter :: half= 0.5e0
        real*8,parameter :: h_ryd_on_2 = h_ryd * half
        real*8 :: energydiff
        real*8 :: temps_ryd(ntemps) 
        real*8 :: oneOverT 
        real*8 :: contribution 
        real*8 :: tfac 
        real*8 :: strength 
        integer :: lower,upper,dd,kk
        temps = (/ 1.80E+03 &  
                  ,4.50E+03 &  
                  ,9.00E+03 &  
                  ,1.80E+04 &  
                  ,4.50E+04 &  
                  ,9.00E+04 &  
                  ,1.80E+05 &  
                  ,4.50E+05 &  
                  ,9.00E+05 &  
                  ,1.80E+06 &  
                  ,4.50E+06 &  
                  ,9.00E+06 &  
                  ,1.80E+07 /) 
        
        temps_ryd = temps * boltz_si  / (electrostat* ryd_ev)
        do dd = 1, nlevels !number of lv numbers

            do lower = 1,nmax 
                a = AARATE_SORTED  (dd,lower)

                do upper = lower+1,nmax

                    energydiff = E_RES_SORTED(dd) - & 
                                 energyFromInput(upper) 
                    b = branching_ratio(dd,upper)
                    w = W_SORTED       (dd)

                    if (energydiff .gt. 0.0d0) then 
                     if ( (a .gt. 0.0d0) .and. (b .gt. 0.0d0)) then
                            strength = a * b * w * h_ryd_on_2
                            do kk = 1, ntemps 
                                oneOverT = 1./temps_ryd(kk)
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
        

        !close(25)

    end subroutine