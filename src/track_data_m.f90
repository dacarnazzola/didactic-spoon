module track_data_m
use, non_intrinsic :: constants_m, only: rk, stdout, ft2nmi, rad2deg
implicit none
private

    type :: sensor_measurement
        real(rk) :: r     !! range [ft]
        real(rk) :: rdot  !! d(range)/d(t) [ft/sec]
        real(rk) :: az    !! azimuth [rad]
!!TODO:        real(rk) :: azdot !! d(azimuth)/d(t) [rad/sec]
        real(rk) :: el    !! elevation [rad]
!!TODO:        real(rk) :: eldot !! d(elevation)/d(t) [rad/sec]
    end type sensor_measurement

    public :: sensor_measurement, print_measurement

    contains


        impure elemental subroutine print_measurement(meas)
            type(sensor_measurement), intent(in) :: meas
            write(stdout,'(a,e13.6,a,f5.1,a)') 'measurement      range: ',meas%r,' ft, ',meas%r*ft2nmi,' nmi'
            write(stdout,'(a,e13.6,a)')        'measurement range-rate: ',meas%rdot,' ft/sec'
            write(stdout,'(a,f8.1,a,f5.1,a)')  'measurement    azimuth: ',meas%az,'     rad, ',meas%az*rad2deg,' deg'
            write(stdout,'(a,f8.1,a,f5.1,a)')  'measurement  elevation: ',meas%el,'     rad, ',meas%el*rad2deg,' deg'
        end subroutine print_measurement


end module track_data_m
