module mday10
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day10

  contains

  subroutine part1()
    type(FormattedFile) :: ffile
    type(String) :: line,op
    type(String), allocatable :: fields(:)
    character :: screen(0:39,6)
    integer(i64) :: icycle,reg,signal,op_counter,op_arg,crt(2),i

    ffile = fopen("data/day10.txt")

    reg = 1
    icycle = 1
    signal = 0

    op = ""
    op_counter = 0
    op_arg = 0
    screen = ' '
    crt = [0,1]

    clock: do

      if (op_counter == 0) then
        if (.not. ffile%scanline()) then
          exit clock
        end if
        line = ffile%text()
        fields = line%split()

        op = fields(1)

        if (op%buf == "noop") then
          op_counter = 1
          op_arg = 0
        else
          op_counter = 2
          op_arg = fields(2)%to_i64()
        end if
      end if

      if (crt(1) >= reg-1 .and. crt(1) <= reg+1) then
        screen(crt(1), crt(2)) = '#'
      end if

      if (mod(icycle - 20,40) == 0) then
        signal = signal + icycle*reg
      end if

      icycle = icycle + 1
      op_counter = op_counter - 1

      if (op_counter == 0) then
        if (op%buf == "addx") then
          reg = reg + op_arg
        end if
      end if

      crt(1) = crt(1) + 1

      if (crt(1) == 40) then
        crt = [0_i64, crt(2) + 1]
      end if


    end do clock

    write(*,*) "Day 10, Part 1",signal
    write(*,*) "Day 10, Part 2"

    do i = 1,6
      write(*,*) screen(:,i)
    end do

  end subroutine

  subroutine part2()
  end subroutine

  subroutine day10()
    call part1()
    call part2()
  end subroutine

end module
