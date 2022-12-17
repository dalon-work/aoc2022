module mday5
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day5

  contains

  subroutine part1()
    type(FormattedFile) :: ffile
    type(String) :: input_stacks(8),line
    type(CharArray) :: stacks(9)
    character :: c
    integer(i64) :: i,j,k
    logical :: has_line

    ffile = fopen("data/day5.txt")

    do i = 1,8
      has_line = ffile%scanline()
      input_stacks(i) = ffile%text()
    end do

    do i = 8,1,-1
      j = 1
      do k=2,9*4,4
        c = input_stacks(i)%buf(k:k)
        if (c /= ' ') then
          call stacks(j)%push(c)
        end if
        j = j+1
      end do
    end do

    do i = 1,2
      has_line = ffile%scanline()
    end do

    do while(ffile%scanline())
      line = ffile%text()
      block
        type(string), allocatable :: words(:)
        integer(i64) :: counts(3),i
        character :: p
        logical :: b
        associate (num_crates => counts(1), from => counts(2), to => counts(3))
          words = line%split()
          counts = words(2::2)%to_i64()

          do i=1,num_crates
            p = stacks(from)%back()
            b = stacks(from)%pop()
            call stacks(to)%push(p)
          end do
        end associate

      end block
    end do

    write(*,*) "Day 5, Part 1 ", stacks%back()
  end subroutine

  subroutine part2()
    type(FormattedFile) :: ffile
    type(String) :: input_stacks(8),line
    type(CharArray) :: stacks(9)
    character :: c
    integer(i64) :: i,j,k
    logical :: has_line

    ffile = fopen("data/day5.txt")

    do i = 1,8
      has_line = ffile%scanline()
      input_stacks(i) = ffile%text()
    end do

    do i = 8,1,-1
      j = 1
      do k=2,9*4,4
        c = input_stacks(i)%buf(k:k)
        if (c /= ' ') then
          call stacks(j)%push(c)
        end if
        j = j+1
      end do
    end do

    do i = 1,2
      has_line = ffile%scanline()
    end do

    do while(ffile%scanline())
      line = ffile%text()
      block
        type(string), allocatable :: words(:)
        integer(i64) :: counts(3),i
        character, allocatable :: p(:)
        associate (num_crates => counts(1), from => counts(2), to => counts(3))
          words = line%split()
          counts = words(2::2)%to_i64()

          p = stacks(from)%back(num_crates)

          call assert( stacks(from)%pop(num_crates) )
          call stacks(to)%push(p)
        end associate
      end block
    end do

    write(*,*) "Day 5, Part 2 ", stacks%back()
  end subroutine

  subroutine day5()
    call part1()
    call part2()
  end subroutine

end module
