module mday9
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  public :: day9

  contains

  integer(i64) function exec_rope_instructions(rope) result(o)
    integer(i64), intent(inout) :: rope(:,:)
    type(FormattedFile) :: ffile
    type(String) :: line
    character :: dir
    integer(i64) :: dir_steps, change(2), i
    type(i64array),target :: tailx, taily

    call tailx%push(0_i64)
    call taily%push(0_i64)

    ffile = fopen("data/day9.txt")

    do while(ffile%scanline()) 
      line = ffile%text()
      read(line%buf,*) dir, dir_steps
      select case (dir)
      case ('R')
        change = [1,0]
      case ('L')
        change = [-1,0]
      case ('U')
        change = [0,1]
      case ('D')
        change = [0,-1]
      case default
        stop "Something went horribly wrong"
      end select

      do i=1,dir_steps
        rope(1,:) = rope(1,:) + change
        call update_rope(rope,tailx,taily)
      end do

    end do

    ! This is horribly inefficient, but with no proper set type, it still works
    block
      logical, allocatable :: tail_map(:,:)
      integer(i64), pointer :: x(:), y(:)

      x => tailx%array()
      y => taily%array()

      allocate( tail_map(minval(x):maxval(x),minval(y):maxval(y)), source = .false. ) 

      do i = 1,size(x)
        tail_map( x(i), y(i) ) = .true.
      end do

      o = count(tail_map)


    end block


  end function

  subroutine part1()
    integer(i64),allocatable :: rope(:,:)
    allocate( rope(2,2), source=0_i64 )
    write(*,*) "Day 9, Part 1", exec_rope_instructions(rope)
  end subroutine

  subroutine part2()
    integer(i64),allocatable :: rope(:,:)
    allocate( rope(10,2), source=0_i64 )
    write(*,*) "Day 9, Part 2", exec_rope_instructions(rope)
  end subroutine

  subroutine update_rope(rope, tailx, taily)
    integer(i64), intent(inout) :: rope(:,:)
    integer(i64) :: nknots,i, distance(2)
    type(i64array), intent(inout) :: tailx, taily
    logical :: tail_moved
    nknots = size(rope,1)

    tail_moved = .true.
    do i=2,nknots
      distance = rope(i-1,:)-rope(i,:)
      if (any(abs(distance) == 2)) then
        where (abs(distance) == 2)
          distance = sign(1_i64,distance)
        end where
        rope(i,:) = rope(i,:) + distance
      else ! the rest of the rope doesn't move
        tail_moved = .false.
        exit
      endif
    end do
     if (tail_moved) then
       call tailx%push( rope(nknots,1))
       call taily%push( rope(nknots,2))
     end if
  end subroutine

  subroutine day9()
    call part1()
    call part2()
  end subroutine

end module
