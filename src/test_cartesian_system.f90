program test_cartesian_system
use, non_intrinsic :: constants_m, only: rk, stdout
use, non_intrinsic :: cartesian_system_m, only: cartesian_state, init_state, cartesian_to_observation, print_state
use, non_intrinsic :: random_m, only: random_uniform, random_normal
use, non_intrinsic :: vector_math_m, only: vmag
use, non_intrinsic :: atmosphere_m, only: mach1
use, non_intrinsic :: track_data_m, only: sensor_measurement, print_measurement
implicit none

    integer, parameter :: n = 1

    type(cartesian_state) :: test(n), stationary_ground_observer, origin_aircraft_observer
    type(sensor_measurement) :: meas
    real(rk) :: x_vals(n), y_vals(n), altitudes(n), headings(n), machs(n)
    integer :: i

    write(stdout,'(a)') 'beginning TEST_CARTESIAN_SYSTEM...'

    call random_uniform(vals=x_vals, n=n, v_min=10.0_rk, v_max=300.0_rk)
    call random_uniform(vals=y_vals, n=n, v_min=-50.0_rk, v_max=50.0_rk)
    call random_uniform(vals=altitudes, n=n, v_min=2000.0_rk, v_max=60000.0_rk)
    call random_normal(vals=headings, n=n, mu=270.0_rk, sig=15.0_rk)
    call random_normal(vals=machs, n=n, mu=0.9_rk, sig=0.1_rk)

    test = init_state(x_nmi=x_vals, y_nmi=y_vals, alt_ft=altitudes, heading_deg=headings, mach=machs)
    stationary_ground_observer = init_state(x_nmi=0.0_rk, y_nmi=0.0_rk, alt_ft=0.0_rk, heading_deg=90.0_rk, mach=0.0_rk)
    origin_aircraft_observer = init_state(x_nmi=0.0_rk, y_nmi=0.0_rk, alt_ft=30000.0_rk, heading_deg=90.0_rk, mach=0.9_rk)

    test(1) = init_state(sqrt(3.0_rk), 1.0_rk, 30000.0_rk, 270.0_rk, 0.9_rk)

    do i=1,n
        write(stdout,'(a)') 'aircraft observer at the origin'
        call print_state(origin_aircraft_observer)
        write(stdout,'(a)') 'test target'
        call print_state(test(1))
        meas = cartesian_to_observation(origin_aircraft_observer, test(i))
        call print_measurement(meas)
        write(stdout,'(a)') ''
    end do

    write(stdout,'(a)') 'completed TEST_CARTESIAN_SYSTEM'
    write(stdout,'(a)') '========================================'

end program test_cartesian_system
