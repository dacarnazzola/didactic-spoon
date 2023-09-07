program test_atmosphere
use, non_intrinsic :: constants_m, only: rk, stdout
use, non_intrinsic :: atmosphere_m, only: mach1

    integer :: i

    write(stdout,'(a)') 'beginning TEST_ATMOSPHERE...'

    do i=0,60,5
        write(stdout,'(a,i2,a,f7.2,a)') 'Mach 1 @ ',i,' kft: ',mach1(real(i*1000.0_rk, rk)),' ft/sec'
    end do

    write(stdout,'(a)') 'completed TEST_ATMOSPHERE'
    write(stdout,'(a)') '========================================'

end program test_atmosphere
