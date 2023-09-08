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


        pure real(rk) function vec3_magnitude(x)
            real(rk), intent(in) :: x(3)
            vec3_magnitude = sqrt(x(1)*x(1) + x(2)*x(2) + x(3)*x(3))
        end function vec3_magnitude


        pure real(rk) function vector_magnitude(x, n)
            real(rk), intent(in) :: x(*)
            integer, intent(in) :: n
            vector_magnitude = sqrt(sum(x(1:n)**2))
        end function vector_magnitude


end module vector_math_m
