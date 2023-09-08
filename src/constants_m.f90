module constants_m
use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128, input_unit, output_unit, error_unit
implicit none
private

    integer, parameter, public :: i8 = int8
    integer, parameter, public :: i16 = int16
    integer, parameter, public :: i32 = int32
    integer, parameter, public :: i64 = int64

    integer, parameter, public :: r32 = real32
    integer, parameter, public :: r64 = real64
    integer, parameter, public :: r128 = real128

    integer, parameter, public :: stdin = input_unit
    integer, parameter, public :: stdout = output_unit
    integer, parameter, public :: stderr = error_unit

    integer, parameter, public :: rk = r64

    !! constants given to 40 significant digits via https://www.wolframalpha.com/
    real(rk), parameter, public ::         pi = 3.141592653589793238462643383279502884197_rk
    real(rk), parameter, public ::       pi_2 = 1.570796326794896619231321691639751442099_rk
    real(rk), parameter, public ::      twopi = 6.283185307179586476925286766559005768394_rk
    real(rk), parameter, public ::    deg2rad = 0.01745329251994329576923690768488612713443_rk
    real(rk), parameter, public ::    rad2deg = 57.29577951308232087679815481410517033241_rk
    real(rk), parameter, public ::     nmi2ft = 6076.115485564304461942257217847769028871_rk
    real(rk), parameter, public ::     ft2nmi = 0.0001645788336933045356371490280777537796976_rk
    real(rk), parameter, public ::         g0 = 32.087686258_rk !! https://en.wikipedia.org/wiki/Gravity_of_Earth

end module constants_m
