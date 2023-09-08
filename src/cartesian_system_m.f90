module cartesian_system_m
use, non_intrinsic :: constants_m, only: rk, nmi2ft, deg2rad, twopi
use, non_intrinsic :: atmosphere_m, only: mach1
implicit none
private

    type :: cartesian_state
        real(rk) :: x(3) = 0.0_rk !! position (ft)
        real(rk) :: v(3) = 0.0_rk !! velocity (ft/sec)
        real(rk) :: a(3) = 0.0_rk !! acceleration (ft/sec**2)
    end type cartesian_state

    public :: cartesian_state, init_state

    contains


        pure elemental function init_state(x_nmi, y_nmi, alt_ft, heading_deg, mach) result(state)
            real(rk), intent(in) :: x_nmi, y_nmi, alt_ft, heading_deg, mach
            type(cartesian_state) :: state
            real(rk) :: speed, ang_rad
            speed = mach*mach1(alt_ft)
            ang_rad = mod(heading_deg*deg2rad, twopi)
            state%x(1) = x_nmi*nmi2ft
            state%x(2) = y_nmi*nmi2ft
            state%x(3) = alt_ft
            state%v(1) = speed*sin(ang_rad) !! Vx
            state%v(2) = speed*cos(ang_rad) !! Vy
            state%v(3) = 0.0_rk
            state%a = 0.0_rk
        end function init_state


end module cartesian_system_m
