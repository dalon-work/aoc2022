module mday8
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day8

  contains

  subroutine part1()
    type(FormattedFile) :: ffile
    type(string), allocatable :: lines(:)
    character, allocatable :: trees(:,:)
    character :: m
    logical, allocatable :: visible(:,:)
    integer(i64) :: i,j,total_visible

    ffile = fopen("data/day8.txt")

    lines = ffile%readlines()

    allocate( trees(size(lines), lines(1)%length()) )

    do i=1,size(lines)
      do j=1,lines(1)%length()
        trees(i,j) = lines(i)%buf(j:j)
      end do
    end do

    visible = trees == 'a' ! always false

    do i=1,size(trees,1)
      m = '0'
      do j=1,size(trees,2)
        if (trees(i,j) > m) then
          visible(i,j) = .true.
          m = trees(i,j)
        end if
      end do
    end do

    do i=1,size(trees,1)
      m = '0'
      do j=size(trees,2),1,-1
        if (trees(i,j) > m) then
          visible(i,j) = .true.
          m = trees(i,j)
        end if
      end do
    end do

    do j=1,size(trees,2)
      m = '0'
      do i=1,size(trees,1)
        if (trees(i,j) > m) then
          visible(i,j) = .true.
          m = trees(i,j)
        end if
      end do
    end do

    do j=1,size(trees,2)
      m = '0'
      do i=size(trees,1),1,-1
        if (trees(i,j) > m) then
          visible(i,j) = .true.
          m = trees(i,j)
        end if
      end do
    end do

    ! Set the edges to true
    visible(1,:) = .true.
    visible(size(trees,1),:) = .true.
    visible(:,1) = .true.
    visible(:,size(trees,2)) = .true.

    total_visible = count(visible)

    write(*,*) "Day 8, Part 1:",total_visible

  end subroutine

  subroutine part2()
  end subroutine

  subroutine day8()
    call part1()
    call part2()
  end subroutine

end module
