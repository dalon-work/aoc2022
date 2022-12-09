module mday4
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day4

  contains

  subroutine part1()
    type(FormattedFile) :: ffile
    type(String) :: line
    integer(i64) :: a(2),b(2),o(2), p1,p2

    ffile = fopen("data/day4.txt")

    p1 = 0
    p2 = 0

    do while (ffile%scanline())
      line = ffile%text()
      call line%replace('-',' ')
      read(line%buf,*) a,b
      o(1) = max(a(1),b(1))
      o(2) = min(a(2),b(2))
      if (all(o == a) .or. all(o == b)) then
        p1 = p1+1
      end if
      if (.not. (b(1) > a(2) .or. a(1) > b(2))) then
        p2 = p2+1
      end if
    end do

    write(*,*) "Day 4, Part 1", p1
    write(*,*) "Day 4, Part 2", p2

  end subroutine

  subroutine part2()
  end subroutine

  subroutine day4()
    call part1()
    call part2()
  end subroutine

end module
