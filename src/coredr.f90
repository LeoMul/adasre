    subroutine coredr( )
        use variables, only:E_RES_SORTED,AARATE_SORTED,&
        drwidth,nlevels,W_SORTED,oiccontinuumground,drcounter,&
        drrate,energyFromInput,nmax,ARRATE_SORTED,angJFromInput,oiccontinuum,grounddiff,thisground,groundFromInput
        use omp_lib
        !add contribution from this block to the upsilons. 
        !this can absolutely be made better
        !this code is incredibly ugly 
        !i think the array accesses are probably quite slow 
        implicit none 
        real*8 :: a,b,w1,w2
        real*8 :: ee 
        real*8,parameter :: boltz_si = 1.38e-23
        real*8,parameter :: electrostat = 1.6e-19 
        real*8,parameter :: ryd_ev = 13.605703976
        real*8,parameter :: h_si  = 6.63e-34  
        real*8,parameter :: h_ryd = h_si / (electrostat* ryd_ev)
        real*8,parameter :: half= 0.5e0
        real*8,parameter :: cs_const = 2.67e-14 !Mb Ry^2 s , adapted from S. Fritzche manual 
        real*8,parameter :: h_ryd_on_2 = h_ryd * half
        real*8 :: energydiff
        integer,parameter::ntemps = 19 
        integer,parameter:: nbin = 10000 
        real*8,parameter :: width = 0.01 !Ry 
        real*8 :: sigma = width/2.355 
        real*8 :: one_over_sigma
        real*8 :: csa(nbin)
        real*8 :: energies(nbin)
        real*8 :: temps (ntemps) 
        real*8 :: temp_ev (ntemps) 
        real*8 :: temps_ryd(ntemps) 
        real*8 :: oneOverTArray(ntemps) 
        real*8 :: oneOverT 
        real*8 :: contribution 
        real*8 :: tfac 
        real*8 :: strength 
        integer :: lower,upper,dd,kk
        real*8,parameter :: kb_ev   = 8.617333262e-5
        real*8,parameter :: ry_ev   = 13.605693122990
        real*8,parameter :: bohr_cm = 5.29177e-9
        real*8,parameter :: bohr_cm2 = bohr_cm * bohr_cm
        real*8,parameter :: pi = 3.14159265359
        real*8 :: constant(ntemps)
        real*8 :: const
        real*8 :: check 
        integer :: ff ,ii,tt 
        character*100 fn , filenumber
        drcounter = drcounter + 1 
        temps = (/ 4.00E+01,&
                   8.00E+01,&
                   2.00E+02,&
                   4.00E+02,&
                   8.00E+02,&
                   2.00E+03,&
                   4.00E+03,&
                   8.00E+03,&
                   2.00E+04,&
                   4.00E+04,&
                   8.00E+04,&
                   2.00E+05,&
                   4.00E+05,&
                   8.00E+05,&
                   2.00E+06,&
                   4.00E+06,&
                   8.00E+06,&
                   2.00E+07,&
                   4.00E+07 /)
!       
!   magic number is soemwhere in adasdr, 
!   array ec is transferred into eac 
!   'correct' energies appear in function fbar... 
!   a correction is made to ec somewhere. must investigate.
!           EC(I)=T2-TCX                         !REMOVE CONT SHIFT FROM ALL
!suspect line ^ 
!

        do ii = 1, nbin 
            energies(ii) = (ii-1) * (2.0 /nbin) + 1e-6 
        end do 
        if (.not. allocated(drrate)) allocate(drrate(ntemps,1))

        temp_ev = temps * kb_ev
        temps_ryd = temps * boltz_si  / (electrostat* ryd_ev) 
        const = 4.0 * pi * bohr_cm2 * ry_ev
        constant = 0.5d0 * (const/ temp_ev) ** 1.5
!
        !!check = 0.0d0
        !!open(345345,file='cont') 
        !!do ii = 1,15 
        !!    check = check + energyFromInput(ii)-groundFromInput - (oiccontinuum(ii) - thisground)
        !!    write(345345,'(5F17.8)'),energyFromInput(ii)-groundFromInput,oiccontinuum(ii)-oiccontinuum(1),oiccontinuum(ii) - thisground,energyFromInput(ii)-groundFromInput - (oiccontinuum(ii) - thisground),thisground
        !!end do 
        !!close(345355)
!
        !!check = check/15
        grounddiff = oiccontinuumground-groundFromInput
        csa =0.0d0 

        !do ii = 1, 1 
            ii=1
            w2 = angJFromInput(ii) + 1 
        !$omp parallel shared(drrate,csa,w2) private(contribution,energydiff,kk,ff,tt,w1,tfac,ee)
        !$omp do schedule(static) !!!
            do kk = 1, nlevels 
                !do ff = 1,nlevels 
                    contribution = AARATE_SORTED(kk,ii)  * drwidth(KK) *w1/w2 
                    energydiff = E_RES_SORTED(kk) - energyFromInput(ii) - grounddiff
                    w1 = W_SORTED(kk) 
                    if (contribution>0 .and.energydiff>0) then 
                        !print '(3ES13.4,2F17.8)',energydiff,AARATE_SORTED(kk,ii),ARRATE_SORTED(kk,ff),groundFromInput-thisground,check
                        csa = csa + contribution * (gaussian(energydiff)/energies)
                        !print*,(gaussian(energydiff)/energies)  
                        !stop
                        do tt =1 ,ntemps 
                            tfac = constant(tt)*contribution
                            ee =  exp(-energydiff/temps_ryd(tt)) 
                            tfac = tfac *ee
                            drrate(tt,ii) = drrate(tt,ii) + tfac 
                            !print*,tfac,contribution,constant(tt),drwidth(KK),ARRATE_SORTED(kk,ff),AARATE_SORTED(kk,ii)
                        end do 
                    end if 
                !end do 
            end do 

        !end do 

        !$OMP END DO
        !$omp end parallel
        write(filenumber,'(I10)') drcounter
        fn = 'drcheck' // adjustl(filenumber)
        !write(fn,'(A7,I10)') 'drcheck',drcounter
        open(7373,file=trim(fn))
        do ii = 1, ntemps 
            write(7373,*) temps(ii),drrate(ii,1)
        end do 
        close(7373) 
        open(7373,file='cs'//trim(fn))
        csa = csa * 0.5 * cs_const
        do ii = 1,nbin 
            write(7373,*) energies(ii),csa(ii)
        end do 
        close(7373)
        !!!do this multiplication here 
        !!oneOverTArray = oneOverTArray * h_ryd_on_2 
        !!!do kk = 1, ntemps 
        !!!    oneOverT = oneOverTArray(kk)
        !!!    write(999,*) upsilon(kk,1,2),oneOverT,upsilon(kk,1,2)*oneOverT
        !!!    upsilon(kk,1,2) = upsilon(kk,1,2) * oneOverT
        !!!    write(999,)
        !!!end do 
        !close(25)

        contains 

        function gaussian(centre)
            real*8 :: gaussian(nbin)
            real*8 :: centre 
            real*8 :: oneOverRoot2pi = 0.3989422804 
            one_over_sigma = 1./sigma 
            gaussian = (one_over_sigma * oneOverRoot2pi) * exp(-(energies - centre)**2 / (2.0 * sigma * sigma)  )

        end function 

    end subroutine