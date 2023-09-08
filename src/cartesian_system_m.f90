module cartesian_system_m
use, non_intrinsic :: constants_m, only: rk, nmi2ft, deg2rad, twopi, g0, rad2deg, ft2nmi, stdout
use, non_intrinsic :: atmosphere_m, only: mach1
use, non_intrinsic :: track_data_m, only: sensor_measurement
use, non_intrinsic :: vector_math_m, only: vmag, vunit
implicit none
private

    type :: cartesian_state
        real(rk) :: x(3) = 0.0_rk !! position (ft)
        real(rk) :: v(3) = 0.0_rk !! velocity (ft/sec)
        real(rk) :: a(3) = 0.0_rk !! acceleration (ft/sec**2)
    end type cartesian_state

    public :: cartesian_state, init_state, heading, cartesian_to_observation, print_state, observation_to_cartesian

    contains


        pure elemental function init_state(x_nmi, y_nmi, alt_ft, heading_deg, mach) result(state)
            real(rk), intent(in) :: x_nmi, y_nmi, alt_ft, heading_deg, mach
            type(cartesian_state) :: state
            real(rk) :: speed, ang_rad
            speed = mach*mach1(alt_ft)
            ang_rad = mod(heading_deg*deg2rad, twopi)
            state%x(1) = x_nmi*nmi2ft
            state%x(2) = y_nmi*nmi2ft
            state%x(3) = alt_ft
            state%v(1) = speed*sin(ang_rad) !! Vx
            state%v(2) = speed*cos(ang_rad) !! Vy
            state%v(3) = 0.0_rk
            state%a = 0.0_rk
        end function init_state


        pure real(rk) function heading(vec2)
            real(rk), intent(in) :: vec2(2)
            heading = atan2(vec2(1), vec2(2))
            if (heading < 0.0_rk) heading = heading + twopi
        end function heading


        pure elemental function cartesian_to_observation(obs, tgt) result(meas)
            type(cartesian_state), intent(in) :: obs, tgt
            type(sensor_measurement) :: meas
            real(rk) :: x_obs_tgt(3), v_obs_tgt(3)
            x_obs_tgt = tgt%x - obs%x
            v_obs_tgt = tgt%v - obs%v
            meas%r = max(vmag(x_obs_tgt), 1.0_rk) !! range - relative to observer position
            meas%rdot = dot_product(v_obs_tgt, x_obs_tgt)/vmag(x_obs_tgt) !! range-rate - relative velocity from observer perspective
            meas%az = heading(x_obs_tgt(1:2)) !! azimuth - relative to observer position, global coordinates
            meas%el = asin(x_obs_tgt(3)/meas%r) !! elevation - relative to observer altitude
        end function cartesian_to_observation


        pure elemental function observation_to_cartesian(obs, meas) result(tgt)
            type(cartesian_state), intent(in) :: obs
            type(sensor_measurement), intent(in) :: meas
            type(cartesian_state) :: tgt
            real(rk) :: x_obs_tgt(3)
            tgt%x = obs%x + az_el_to_vec(meas%az, meas%el)*meas%r
            x_obs_tgt = tgt%x - obs%x
            tgt%v = (dot_product(obs%v, x_obs_tgt)/vmag(x_obs_tgt) + meas%rdot)*vunit(x_obs_tgt)
            tgt%a = 0.0_rk
        end function observation_to_cartesian


        pure function az_el_to_vec(az, el) result(vec)
            real(rk), intent(in) :: az, el
            real(rk) :: vec(3)
            vec(1) = sin(az)
            vec(2) = cos(az)
            vec(3) = sin(el)
            vec = vunit(vec)
        end function az_el_to_vec


        impure elemental subroutine print_state(state)
            type(cartesian_state), intent(in) :: state
            write(stdout,'(a,3e14.6,a,2f7.1,f9.1)') 'Position [ft]: ',state%x,', [nmi, nmi, kft]: ',state%x(1:2)*ft2nmi, &
                                                    state%x(3)/1000.0_rk
            write(stdout,'(a,3e14.6,a,f5.2,a,f6.1)') 'Velocity [ft/sec]: ',state%v,', Speed [mach]: ', &
                                                     vmag(state%v)/mach1(state%x(3)),', Heading [deg]: ', &
                                                     heading(state%v(1:2))*rad2deg
            write(stdout,'(a,3e14.6,a,f5.2)') 'Acceleration [ft/sec**2]: ',state%a,', G''s: ',vmag(state%a)/g0
        end subroutine print_state


end module cartesian_system_m
