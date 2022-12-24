module mday7
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day7

  contains

  subroutine part1()
    type(FormattedFile) :: ffile
    type(string) :: line
    type(i64Array) :: bytes, all_dir_sizes
    integer(i64) :: total

    ffile = fopen("data/day7.txt")

    total = 0

    do while (ffile%scanline())
      line = ffile%text()
      if (line%buf == "$ cd ..") then
        call all_dir_sizes%push(bytes%back())
        if ( bytes%back() <= 100000 ) then
          total = total + bytes%back()
        end if
        call assert( bytes%pop() )
      else if (line%buf == "$ ls") then
        cycle
      else if (line%buf(1:4) == "$ cd") then
        call bytes%push(0_i64)
      else if (line%buf(1:3) == "dir") then
        cycle
      else
        bytes%array() = bytes%array() + line%to_i64()
      end if
    end do

    do while (bytes%size() > 0)
      call all_dir_sizes%push(bytes%back())
      call assert(bytes%pop())
    end do

    write(*,*) "Day 7, Part 1:",total

    call part2(all_dir_sizes)

  end subroutine

  subroutine part2(all_dir_sizes)
    type(i64Array), target, intent(inout) :: all_dir_sizes
    integer(i64), pointer :: dirs(:)
    integer(i64), parameter :: MAX_MEM = 70000000
    integer(i64), parameter :: NEEDED_MEM = 30000000
    integer(i64) :: unused, smallest

    unused = MAX_MEM - all_dir_sizes%back()

    dirs => all_dir_sizes%array()

    smallest = minval(dirs, (dirs >= NEEDED_MEM - unused))

    write(*,*) "Day 7, Part 2:", smallest

  end subroutine

  subroutine day7()
    call part1()
  end subroutine

end module
