module mday3
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day3

  contains

    integer(i64) function char_to_priority(c) result(i)
      character, intent(in) :: c
      if (is_upper(c)) then
        i = (iachar(c) - iachar('A'))+27
      else
        i = (iachar(c) - iachar('a'))+1
      end if
    end function

  integer(i64) function rucksack(items)
    type(string), intent(in) :: items
    integer(i64) :: i

    rucksack = 0

    do i= 1, items%length()
      rucksack = ibset(rucksack,char_to_priority(items%buf(i:i))-1)
    end do
    end function

  subroutine part1()
    type(FormattedFile) :: ffile
    type(String) :: line
    type(String) :: left, right
    integer(i64) :: mid, sackl, sackr, total, priority

    ffile = fopen("data/day3.txt")

    total = 0

    do while (ffile%scanline())
      line = ffile%text()
      mid = line%length() / 2
      left = line%buf(:mid)
      right = line%buf(mid+1:)
      sackl = rucksack(left)
      sackr = rucksack(right)
      priority = trailz(iand(sackl, sackr))+1
      total = total + priority
    end do

    write(*,*) "Day 3, Part 1", total

  end subroutine

  subroutine part2()
    type(FormattedFile) :: ffile
    type(String), allocatable :: lines(:)
    integer(i64) :: sack(3), total, priority,i,j

    ffile = fopen("data/day3.txt")

    total = 0

    lines = ffile%readlines()

    do i=1,size(lines),3
      sack(1) = rucksack(lines(i+0))
      sack(2) = rucksack(lines(i+1))
      sack(3) = rucksack(lines(i+2))
      priority = trailz(iand(iand(sack(1), sack(2)), sack(3)))+1
      total = total + priority
    end do

    write(*,*) "Day 3, Part 2", total

  end subroutine

  subroutine day3()
    call part1()
    call part2()
  end subroutine

end module
