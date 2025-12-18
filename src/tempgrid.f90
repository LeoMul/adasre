module tempgrid 
    use kinds 
    implicit none 
    real(floatKind), allocatable :: temps_kelvin(:)
    integer(4) :: ntemps = 9
    integer(4) :: ntemps_kne = 13 
!    
    contains 
!
    subroutine knetempgrid
        implicit none 
        if(allocated(temps_kelvin)) deallocate(temps_kelvin)
        allocate(temps_kelvin(ntemps_kne))
        ntemps = ntemps_kne
        temps_kelvin = (/ 1.80E+03 &  
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
    end subroutine
!
    subroutine defaulttempgrid
        ntemps = 9 
        if(allocated(temps_kelvin)) deallocate(temps_kelvin)
        allocate(temps_kelvin(ntemps))    

        temps_kelvin = (/ & 
           4.00E+02 &  
          ,6.00E+02 &  
          ,9.00E+02 &  
          ,1.35E+03 &  
          ,2.00E+03 &  
          ,3.00E+03 &  
          ,4.50E+03 &  
          ,6.70E+03 &  
          ,1.00E+04 & 
          /) 
          temps_kelvin = temps_kelvin * (26 ** 3)
    end subroutine

    subroutine readtempgrid(temps)
        integer :: temps 
        integer :: ii 
        if(allocated(temps_kelvin)) deallocate(temps_kelvin)
        ntemps = temps
        allocate(temps_kelvin(temps))
        read(50,*) (temps_kelvin(ii),ii=1,ntemps)
    end subroutine

end module tempgrid 