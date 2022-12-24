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

    call part2(trees)

  end subroutine

  integer(i64) function char_to_index(c) result(o)
    character, intent(in) :: c
    o = iachar(c) - iachar('0') + 1
  end function

  subroutine part2(trees)
    character, intent(in) :: trees(:,:)
    integer(i64) :: loc(10), cindex, last_seen
    integer(i64), allocatable :: visibility(:,:)
    integer(i64) :: i,j

    allocate( visibility(size(trees,1),size(trees,2)), source=1_i64)

    do i=1,size(trees,1)
      loc = 1
      do j=1,size(trees,2)
        cindex = char_to_index(trees(i,j))
        visibility(i,j) = visibility(i,j) * (j - maxval(loc(cindex:)))
        loc(cindex) = j
      end do
    end do

    do i=1,size(trees,1)
      loc = size(trees,2)
      do j=size(trees,2),1,-1
        cindex = char_to_index(trees(i,j))
        visibility(i,j) = visibility(i,j) * (minval(loc(cindex:))-j)
        loc(cindex) = j
      end do
    end do

    do j=1,size(trees,2)
      loc = 1
      do i=1,size(trees,1)
        cindex = char_to_index(trees(i,j))
        visibility(i,j) = visibility(i,j) * (i - maxval(loc(cindex:)))
        loc(cindex) = i
      end do
    end do

    do j=1,size(trees,2)
      loc = size(trees,1)
      do i=size(trees,1),1,-1
        cindex = char_to_index(trees(i,j))
        visibility(i,j) = visibility(i,j) * (minval(loc(cindex:))-i)
        loc(cindex) = i
      end do
    end do

    write(*,*) "Day 8, Part 2", maxval(visibility)

  end subroutine

  subroutine day8()
    call part1()
  end subroutine

end module
