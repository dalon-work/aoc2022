module mday2
  use aoc_utils
  use iso_fortran_env
  implicit none
  private

  character, parameter :: abc(*) = ['A', 'B', 'C']
  character, parameter :: xyz(*) = ['X', 'Y', 'Z']
                                                     ! A  B  C
  integer(i64), parameter :: score(*,*) = reshape( [ 3, 0, 6 , & ! X
                                                       6, 3, 0 , & ! Y
                                                       0, 6, 3 ],& ! Z 
                                                     [3,3])   
  integer(i64), parameter :: lose_draw_win(*) = [ 0, 3, 6 ]
  public :: day2

  contains

  subroutine part1()
    type(FormattedFile) :: ffile
    type(String) :: line
    integer(i64) :: my_score
    character :: them, me
    integer :: i,j

    ffile = fopen("data/day2.txt")

    my_score = 0

    do while (ffile%scanline())
      line = ffile%text()
      read(line%buf,*) them, me
      i = findloc(abc,them,1)
      j = findloc(xyz,me,1)
      my_score = my_score + score(i,j) + j
    end do

    write(*,*) "Day 2 Part 1", my_score

  end subroutine

  subroutine part2()
    type(FormattedFile) :: ffile
    type(String) :: line
    integer(i64) :: my_score,i,j,k
    character :: them, me

    ffile = fopen("data/day2.txt")

    my_score = 0

    do while (ffile%scanline())
      line = ffile%text()
      read(line%buf,*) them, me
      i = findloc(abc,them,1)
      j = lose_draw_win(findloc(xyz,me,1))
      k = findloc(score(i,:),j,1)
      my_score = my_score + j + k
    end do

    write(*,*) "Day 2 Part 2", my_score

  end subroutine

  subroutine day2()
    call part1()
    call part2()
  end subroutine

end module
