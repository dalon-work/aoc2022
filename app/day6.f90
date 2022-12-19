module mday6
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day6

  contains

  integer(i64) function find_non_repeating_sequence(buf, m) result(o)
    character(*), intent(in) :: buf
    integer(i64), intent(in) :: m
    integer(i64) :: mask,i,j,k,n,ci
    character :: c
    logical :: found_repeat

    o = 0

    n = len(buf)
    mask = 0
    do i = 1,n-m+1
      mask = 0
      found_repeat = .false.
      do j = i,i+m-1
        c = buf(j:j)
        ci = iachar(c) - iachar('a')
        if (btest(mask,ci)) then
          found_repeat = .true.
          exit
        end if
        mask = ibset(mask,ci)
      end do
      if (.not. found_repeat) then
        o = i+m-1
        exit
      end if
    end do

  end function

  subroutine part1()
    type(FormattedFile) :: ffile
    type(string) :: s

    ffile = fopen("data/day6.txt")

    call assert(ffile%scanline())

    s = ffile%text()
    write(*,*) "Day 6, Part 1: ", find_non_repeating_sequence(s%buf, 4_i64)
    write(*,*) "Day 6, Part 2: ", find_non_repeating_sequence(s%buf, 14_i64)

  end subroutine

  subroutine part2()
  end subroutine

  subroutine day6()
    call part1()
    call part2()
  end subroutine

end module
