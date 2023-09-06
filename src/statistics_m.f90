module statistics_m
use, non_intrinsic :: constants_m, only: rk
implicit none
private

    public :: avg
    public :: std

    contains


        pure real(rk) function avg(x)
            real(rk), intent(in) :: x(:)
            integer :: n
            n = size(x)
            if (n > 0) then
                avg = sum(x)/real(n, rk)
            else
                avg = 0.0_rk
            end if
        end function avg


        pure real(rk) function std(x)
            real(rk), intent(in) :: x(:)
            integer :: n
            n = size(x)
            if (n > 1) then
                std = sqrt(sum((x - avg(x))**2)/real(n - 1, rk))
            else
                std = 0.0_rk
            end if
        end function std


end module statistics_m
