    subroutine resonantUpsilon( )
        use variables, only:temps,E_RES_SORTED,energyNstates,AARATE_SORTED,branching_ratio,nlevels,ntemps,numberContinuum,W_SORTED,upsilon,energyFromInput,groundFromInput
        !add contribution from this block to the upsilons. 

        implicit none 
        real*8 :: sum 
        real*8 :: a,b,w

        real*8,parameter :: boltz_si = 1.38e-23
        real*8,parameter :: electrostat = 1.6e-19 
        real*8,parameter :: ryd_ev = 13.605703976
        real*8,parameter :: h_si  = 6.63e-34  
        real*8,parameter :: h_ryd = h_si / (electrostat* ryd_ev)
        real*8,parameter :: half= 0.5e0
        real*8 :: energydiff
        real*8 :: temps_ryd(ntemps) 
        real*8 :: oneOverT 
        real*8 :: contribution 
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

        do kk = 1, ntemps 
            oneOverT = 1./temps_ryd(kk)

            do lower = 1,numberContinuum 
                do upper = lower+1,numberContinuum
                    sum = 0.0d0 
                    !write(25,*) 'Transition ',lower,' to ',upper
                    
                    do dd = 1, nlevels !number of lv numbers
                        !energydiff = E_RES_SORTED(dd) - energyNstates(lower)
                        !energydiff =  energydiff  - (energyNstates(upper) - energyNstates(lower))
                        energydiff = E_RES_SORTED(dd) - energyFromInput(upper) 
                        a = AARATE_SORTED  (dd,lower)
                        b = branching_ratio(dd,upper)
                        w = W_SORTED       (dd)
                        if (energydiff .gt. 0.0d0) then 
                        if ( (a .gt. 0.0d0) .and. (b .gt. 0.0d0)) then
                            contribution = a * b * w * half * h_ryd * exp(- energydiff * oneOverT )* oneOverT
                            if ( ((lower.eq.1) .and. (upper.eq.3)) .and. (kk.eq.6)) then 

                            write(25,'(3I5,4ES10.3,3ES13.6)') lower,upper,dd,a,b,w,energydiff,E_RES_SORTED(dd)-groundFromInput,energyFromInput(upper)-energyFromInput(1),energyNstates(1)
                            write(25,*) 'check ', oneOverT/13.606 , energydiff*13.606,h_ryd*13.606,contribution,nlevels 
                            if (contribution .gt. 1e-1) write(25,*) 'huh'
                            end if 

                            sum = sum +  contribution

                        end if
                        end if 
                    end do 

                    upsilon(kk,lower,upper) = upsilon(kk,lower,upper) + sum 
                end do 
            end do 
        end do 
        

        !close(25)

    end subroutine