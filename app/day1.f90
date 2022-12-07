module mday1
  use aoc_utils
  use iso_fortran_env
  implicit none

  contains

  subroutine day1()
    type(FormattedFile) :: ffile
    type(String) :: line
    integer(int64) :: i, cur_elf
    integer(int64), allocatable :: elves(:)

    ffile = fopen("data/day1.txt")

    cur_elf = 0

    allocate(elves(0))
    do while (ffile%scanline())
      line = ffile%text()
      if (line%length() == 0) then
        elves = [elves, cur_elf]
        cur_elf = 0
      else
        i = line%to_int64()
        cur_elf = cur_elf + i
      end if
    end do

    write(*,*) "Day 1 Part 1", maxval(elves)

    part2: block
      logical :: mask(size(elves))

      mask = .true.
      mask(maxloc(elves)) = .false.
      mask(maxloc(elves,mask)) = .false.
      mask(maxloc(elves,mask)) = .false.

      write(*,*) "Day 1 Part 2", sum( elves, mask=.not. mask)

    end block part2

  end subroutine

end module
