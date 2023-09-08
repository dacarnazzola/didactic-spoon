module vector_math_m
use, non_intrinsic :: constants_m, only: rk, twopi
implicit none
private

    interface vmag
        module procedure vec3_magnitude
        module procedure vector_magnitude
    end interface vmag

    public :: vmag, heading

    contains


        pure real(rk) function vec3_magnitude(vec3)
            real(rk), intent(in) :: vec3(3)
            vec3_magnitude = sqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))
        end function vec3_magnitude


        pure real(rk) function vector_magnitude(vec, n)
            real(rk), intent(in) :: vec(*)
            integer, intent(in) :: n
            vector_magnitude = sqrt(sum(vec(1:n)**2))
        end function vector_magnitude


        pure real(rk) function heading(vec2)
            real(rk), intent(in) :: vec2(2)
            heading = atan2(vec2(1), vec2(2))
            if (heading < 0.0_rk) heading = heading + twopi
        end function heading


end module vector_math_m
