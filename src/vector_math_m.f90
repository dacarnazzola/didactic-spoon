module vector_math_m
use, non_intrinsic :: constants_m, only: rk
implicit none
private

    interface vmag
        module procedure vec3_magnitude
        module procedure vector_magnitude
    end interface vmag

    public :: vmag

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


end module vector_math_m
