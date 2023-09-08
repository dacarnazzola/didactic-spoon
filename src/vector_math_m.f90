module vector_math_m
use, non_intrinsic :: constants_m, only: rk
implicit none
private

    interface vmag
        module procedure vec3_magnitude
        module procedure vector_magnitude
    end interface vmag

    interface vunit
        module procedure vec3_unit
        module procedure vector_unit
    end interface vunit

    public :: vmag, vunit

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


        pure function vec3_unit(vec3) result(unit)
            real(rk), intent(in) :: vec3(3)
            real(rk) :: unit(3)
            real(rk) :: work
            work = vmag(vec3)
            if (work > 0.0_rk) then
                unit = vec3/work
            else
                unit = 0.0_rk
            end if 
        end function vec3_unit


        pure function vector_unit(vec, n) result(unit)
            real(rk), intent(in) :: vec(*)
            integer, intent(in) :: n
            real(rk) :: unit(n)
            real(rk) :: work
            work = vmag(vec)
            if (work > 0.0_rk) then
                unit(1:n) = vec(1:n)/work
            else
                unit(1:n) = 0.0_rk
            end if
        end function vector_unit


end module vector_math_m
