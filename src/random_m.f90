module random_m
use, non_intrinsic :: constants_m, only: rk, pi
implicit none
private

    public :: randrng
    public :: normrnd

    contains


        impure subroutine randrng(vals, n, v_min, v_max)
            real(rk), intent(out) :: vals(*)
            integer, intent(in) :: n
            real(rk), intent(in) :: v_min, v_max
            call random_number(vals(1:n))
            vals(1:n) = vals(1:n)*(v_max - v_min) + v_min
        end subroutine randrng


        impure subroutine normrnd(vals, n, mu, sig)
            real(rk), intent(out) :: vals(*)
            integer, intent(in) :: n
            real(rk), intent(in) :: mu, sig
            real(rk) :: u((n+1)/2), v((n+1)/2), r((n+1)/2)
            integer :: nu
            call random_number(u)
            call random_number(v)
            u = 1.0_rk - u
            r = sig*sqrt(-2.0_rk*log(u))
            nu = size(u)
            vals(1:nu) = mu + r*sin(2.0_rk*pi*v)
            if (n > (nu+1)) vals(nu+1:n) = mu + r(1:(n-nu))*cos(2.0_rk*pi*v(1:(n-nu)))
        end subroutine normrnd


end module random_m
