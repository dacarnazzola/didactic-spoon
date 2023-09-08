module atmosphere_m
use, non_intrinsic :: constants_m, only: rk
implicit none
private

    public :: mach1

    real(rk), parameter :: gamma_perf = 1.403_rk !! https://www.engineeringtoolbox.com/air-specific-heat-capacity-d_705.html
    real(rk), parameter ::      r_air = 1716.46_rk !! https://en.wikipedia.org/wiki/Gas_constant

    contains


        pure elemental real(rk) function calorically_imperfect_gamma(T)
            real(rk), intent(in) :: T !! temperature (R)
            real(rk) :: theta_over_T, gamma_minus_one
            theta_over_T = 5500.0_rk/T
            gamma_minus_one = gamma_perf - 1.0_rk
            calorically_imperfect_gamma = 1.0_rk + gamma_minus_one/(1.0_rk + &
                                          gamma_minus_one*(theta_over_T**2*exp(theta_over_T)/(exp(theta_over_T)-1.0_rk)**2))
        end function calorically_imperfect_gamma


        pure elemental real(rk) function rankine(deg_f)
            real(rk), intent(in), value :: deg_f
            rankine = deg_f + 459.67_rk !! https://www.engineeringtoolbox.com/temperature-d_291.html
        end function rankine


        pure elemental real(rk) function mach1(h)
        !! https://www.grc.nasa.gov/www/k-12/rocket/atmos.html
            real(rk), intent(in), value :: h !! altitude (ft)
            real(rk) :: T_f, & !! temperature (F)
                        T_r    !! temperature (R)
            if (h <= 36152.0_rk) then !! Troposphere
                T_f = 59.0_rk - 0.00356_rk*h
            else if (h <= 82345.0_rk) then !! Lower Stratosphere
                T_f = -70.0_rk
            else !! Upper Stratosphere
                T_f = -205.05_rk + 0.00164_rk*h
            end if
            T_r = rankine(T_f)
            mach1 = sqrt(calorically_imperfect_gamma(T_r)*r_air*T_r)
        end function mach1

        
end module atmosphere_m
