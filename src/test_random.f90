program test_random
use, non_intrinsic :: constants_m, only: rk, stdout
use, non_intrinsic :: random_m, only: randrng, normrnd
use, non_intrinsic :: statistics_m, only: avg, std
implicit none

    integer, parameter :: n = 10000000
    real(rk) :: x(n)

    write(stdout,'(a)') 'beginning TEST_RANDOM...'
    write(stdout,'(a,i0)') 'n: ',n

    write(stdout,'(a)')       '    testing randrng, expect min: 10.0, max: 30.0, avg: 20.0'
    call randrng(vals=x, n=n, v_min=10.0_rk, v_max=30.0_rk)
    write(stdout,'(3(a,f0.1))') '        randrng min: ',minval(x),', max: ',maxval(x),', avg: ',avg(x)

    write(stdout,'(a)')       '    testing normrnd, expect avg=19.93, std=8.31'
    call normrnd(vals=x, n=n, mu=19.93_rk, sig=8.31_rk)
    write(stdout,'(2(a,f0.2))') '        normrnd avg: ',avg(x),', std: ',std(x)

    write(stdout,'(a)') 'completed TEST_RANDOM'
    write(stdout,'(a)') '========================================'

end program test_random
