module constants 
    use kinds 
!   Numbers:
    real(floatKind), parameter :: pi          = 3.14159265359d0
    real(floatKind), parameter :: overpi      = 1./pi 
    real(floatKind), parameter :: half        = 0.5d0
!   Physics SI:
    real(floatKind), parameter :: boltz_si    = 1.380649d-23
    real(floatKind), parameter :: esu         = 1.60217663d-19
    real(floatKind), parameter :: h_si        = 6.62607015d-34
!   Conversion:
!   Physics other units:
    real(floatKind), parameter :: ryd_ev      = 13.605703976d0
    real(floatKind), parameter :: SItoRYD     = 1d0/(esu * ryd_ev)
    real(floatKind), parameter :: h_ryd       = h_si * SItoRYD
    real(floatKind), parameter :: h_ryd_on_2  = h_ryd * half 
    real(floatKind), parameter :: hbar_ryd    = h_ryd_on_2 * overpi
    real(floatKind), parameter :: boltz_ryd   = boltz_si * SItoRYD
end module 