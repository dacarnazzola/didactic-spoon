program test_cartesian_system
use, non_intrinsic :: constants_m, only: rk, stdout, g0, ft2nmi
use, non_intrinsic :: cartesian_system_m, only: cartesian_state, init_state
use, non_intrinsic :: random_m, only: random_uniform, random_normal
use, non_intrinsic :: vector_math_m, only: vmag
use, non_intrinsic :: atmosphere_m, only: mach1
implicit none

    integer, parameter :: n = 5

    type(cartesian_state) :: test(n)
    real(rk) :: x_vals(n), y_vals(n), altitudes(n), headings(n), machs(n)
    integer :: i

    write(stdout,'(a)') 'beginning TEST_CARTESIAN_SYSTEM...'

    call random_uniform(vals=x_vals, n=n, v_min=10.0_rk, v_max=300.0_rk)
    call random_uniform(vals=y_vals, n=n, v_min=-50.0_rk, v_max=50.0_rk)
    call random_uniform(vals=altitudes, n=n, v_min=2000.0_rk, v_max=60000.0_rk)
    call random_normal(vals=headings, n=n, mu=270.0_rk, sig=15.0_rk)
    call random_normal(vals=machs, n=n, mu=0.9_rk, sig=0.1_rk)

    test = init_state(x_nmi=x_vals, y_nmi=y_vals, alt_ft=altitudes, heading_deg=headings, mach=machs)

    do i=1,n
        write(stdout,'(a,i0,a)')                'Entity ',i,':'
        write(stdout,'(a,3e14.6,a,2f7.1,f9.1)') '               Position [ft]: ',test(i)%x,', [nmi, nmi, kft]: ', &
                                                                                 test(i)%x(1:2)*ft2nmi,test(i)%x(3)/1000.0_rk
        write(stdout,'(a,3e14.6,a,f5.2)')       '           Velocity [ft/sec]: ',test(i)%v,',    Speed [mach]: ', &
                                                                                 vmag(test(i)%v)/mach1(test(i)%x(3))
        write(stdout,'(a,3e14.6,a,f5.2)')       '    Acceleration [ft/sec**2]: ',test(i)%a,',             G''s: ', &
                                                                                 vmag(test(i)%a)/g0
        write(stdout,'(a)') ''
    end do

    write(stdout,'(a)') 'completed TEST_CARTESIAN_SYSTEM'
    write(stdout,'(a)') '========================================'

end program test_cartesian_system
