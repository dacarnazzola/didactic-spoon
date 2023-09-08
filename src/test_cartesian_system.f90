program test_cartesian_system
use, non_intrinsic :: constants_m, only: rk, stdout, deg2rad
use, non_intrinsic :: cartesian_system_m, only: cartesian_state, init_state, print_state, cartesian_to_observation, &
                                                observation_to_cartesian
use, non_intrinsic :: vector_math_m, only: vmag
use, non_intrinsic :: atmosphere_m, only: mach1
use, non_intrinsic :: track_data_m, only: sensor_measurement, print_measurement
implicit none

    type(cartesian_state) :: obs, tgt
    type(sensor_measurement) :: meas
    integer :: i

    write(stdout,'(a)') 'beginning TEST_CARTESIAN_SYSTEM...'

    obs = init_state(x_nmi=0.0_rk, y_nmi=0.0_rk, alt_ft=30000.0_rk, heading_deg=0.0_rk, mach=0.9_rk)
    write(stdout,'(a)') 'OBSERVER'
    call print_state(obs)
    write(stdout,'(a)') ''

    do i=0,360,30
        write(stdout,'(a,i0,a)') 'TARGET at ',i,' deg'
        tgt = init_state(10.0_rk*sin(real(i, rk)*deg2rad), 10.0_rk*cos(real(i, rk)*deg2rad), 30000.0_rk, 180.0_rk, 0.9_rk)
        call print_state(tgt)
        write(stdout,'(a)') ''

        write(stdout,'(a)') 'OBSERVER --> TARGET: MEASUREMENT'
        meas = cartesian_to_observation(obs, tgt)
        call print_measurement(meas)
        write(stdout,'(a)') ''

        write(stdout,'(a)') 'MEASUREMENT --> TARGET: TRACK'
        tgt = observation_to_cartesian(obs, meas)
        call print_state(tgt)
        write(stdout,'(a)') ''

        write(stdout,'(a)') '-----'
        write(stdout,'(a)') ''
    end do

    write(stdout,'(a)') 'completed TEST_CARTESIAN_SYSTEM'
    write(stdout,'(a)') '========================================'

end program test_cartesian_system
