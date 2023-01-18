module mday11
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day11

  contains

  integer(i64) function op(m, old) result(new)
    integer(i64), intent(in) :: m, old
    select case(m)
    case(0)
       new = old*13
    case(1)
       new = old + 4
    case(2)
      new = old * 11
    case(3)
      new = old + 8
    case(4)
      new = old * old
    case(5)
      new = old + 5
    case(6)
      new = old + 1
    case(7)
      new = old + 3
    case default
      stop "Bad monkey!"
    end select
  end function

  integer(i64) function throw(m, val)
    integer(i64), intent(in) :: m, val
    integer(i64), parameter  :: div(0:7) = [ integer(i64) :: 11, 17, 5, 13, 19, 2, 3, 7]
    integer(i64), parameter  :: true(0:7) = [ integer(i64) :: 4, 2, 6, 1, 3, 7, 0, 4]
    integer(i64), parameter  :: false(0:7) = [ integer(i64) :: 7, 6, 5, 2, 1, 0, 5, 3]

    if (mod(val, div(m)) == 0) then
      throw = true(m)
    else
      throw = false(m)
    end if
  end function

  subroutine monkey_inspect(monkeys, m, inspect)
    type(i64array), intent(inout) :: monkeys(0:7)
    integer(i64), intent(inout) :: inspect(0:7)
    integer(i64), intent(in) :: m
    integer(i64) :: i, new, n

    associate (items => monkeys(m)%array())
      do i = 1, size(items)
        new = op(m, items(i))
        new = new / 3
        n = throw(m, new)
        call monkeys(n)%push(new)
        inspect(m) = inspect(m) + 1
      end do
      call monkeys(m)%clear()
    end associate
  end subroutine

  subroutine monkey_inspect2(monkeys, m, inspect)
    type(i64array), intent(inout) :: monkeys(0:7)
    integer(i64), intent(inout) :: inspect(0:7)
    integer(i64), intent(in) :: m
    integer(i64) :: i, new, n
    integer(i64), parameter :: top = product([11, 17, 5, 13, 19, 2, 3, 7])

    associate (items => monkeys(m)%array())
      do i = 1, size(items)
        new = op(m, items(i))
        new = mod(new,top)
        n = throw(m, new)
        call monkeys(n)%push(new)
        inspect(m) = inspect(m) + 1
      end do
      call monkeys(m)%clear()
    end associate
  end subroutine

  subroutine part1()
    type(i64array) :: monkeys(0:7)
    integer(i64) :: i,m, inspect(0:7)
    monkeys(0) = [integer(i64) :: 98, 97, 98, 55, 56, 72]
    monkeys(1) = [integer(i64) :: 73, 99, 55, 54, 88, 50, 55]
    monkeys(2) = [integer(i64) :: 67, 98]
    monkeys(3) = [integer(i64) :: 82, 91, 92, 53, 99]
    monkeys(4) = [integer(i64) :: 52, 62, 94, 96, 52, 87, 53, 60]
    monkeys(5) = [integer(i64) :: 94, 80, 84, 79]
    monkeys(6) = [integer(i64) :: 89]
    monkeys(7) = [integer(i64) :: 70, 59, 63]
    inspect = 0

    do i=1,20
      do m=0,7
        call monkey_inspect(monkeys, m, inspect)
      end do
    end do

    block
      logical :: mask(8)
      mask = .true.

      mask( maxloc(inspect, mask) ) = .false.
      mask( maxloc(inspect, mask) ) = .false.

      write(*,*) "Day 11, Part 1:", product( inspect, .not. mask )
    end block
  end subroutine

  subroutine part2()
    type(i64array) :: monkeys(0:7)
    integer(i64) :: i,m, inspect(0:7)
    monkeys(0) = [integer(i64) :: 98, 97, 98, 55, 56, 72]
    monkeys(1) = [integer(i64) :: 73, 99, 55, 54, 88, 50, 55]
    monkeys(2) = [integer(i64) :: 67, 98]
    monkeys(3) = [integer(i64) :: 82, 91, 92, 53, 99]
    monkeys(4) = [integer(i64) :: 52, 62, 94, 96, 52, 87, 53, 60]
    monkeys(5) = [integer(i64) :: 94, 80, 84, 79]
    monkeys(6) = [integer(i64) :: 89]
    monkeys(7) = [integer(i64) :: 70, 59, 63]
    inspect = 0

    do i=1,10000
      do m=0,7
        call monkey_inspect2(monkeys, m, inspect)
      end do
    end do

    block
      logical :: mask(8)
      mask = .true.

      mask( maxloc(inspect, mask) ) = .false.
      mask( maxloc(inspect, mask) ) = .false.

      write(*,*) "Day 11, Part 2:", product( inspect, .not. mask )
    end block
  end subroutine

  subroutine day11()
    call part1()
    call part2()
  end subroutine

end module
