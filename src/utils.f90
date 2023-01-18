module aoc_utils
  use iso_fortran_env
  implicit none

  integer, parameter :: i64 = int64
  integer, parameter :: r64 = real64

  type :: FormattedFile
    private
    integer :: funit
    character(:), allocatable :: buf

    contains
      procedure, public, non_overridable :: scanline
      procedure, public, non_overridable :: readlines
      procedure, public, non_overridable :: text
  end type

  type :: String
    character(:), allocatable :: buf
    contains
      procedure, public, non_overridable :: length
      procedure, public, non_overridable :: to_i64
      procedure, public, non_overridable :: replace
      procedure, public, non_overridable :: split
  end type

  real, parameter :: grow = 1.618
  type :: CharArray
    integer(i64) :: n = 0
    character, allocatable :: buf(:)
    contains
      procedure, private, non_overridable :: char_array_push
      procedure, private, non_overridable :: char_array_push_many
      generic, public :: push => char_array_push, char_array_push_many

      procedure, private, non_overridable :: char_array_pop
      procedure, private, non_overridable :: char_array_pop_many
      generic, public :: pop => char_array_pop, char_array_pop_many

      procedure, private, non_overridable :: char_array_back
      procedure, private, non_overridable :: char_array_back_many
      generic, public :: back => char_array_back, char_array_back_many

      procedure, public, non_overridable :: size => char_array_size
      procedure, public, non_overridable :: capacity => char_array_capacity
      procedure, public, non_overridable :: clear => char_array_clear
  end type

  type :: i64Array
    integer(i64) :: n = 0
    integer(i64), allocatable :: buf(:)
    contains
      procedure, private, non_overridable :: i64_array_push
      procedure, private, non_overridable :: i64_array_push_many
      generic, public :: push => i64_array_push, i64_array_push_many

      procedure, private, non_overridable :: i64_array_pop
      procedure, private, non_overridable :: i64_array_pop_many
      generic, public :: pop => i64_array_pop, i64_array_pop_many

      procedure, private, non_overridable :: i64_array_back
      procedure, private, non_overridable :: i64_array_back_many
      generic, public :: back => i64_array_back, i64_array_back_many

      procedure, public, non_overridable :: array => i64_array_array
      procedure, public, non_overridable :: size => i64_array_size
      procedure, public, non_overridable :: capacity => i64_array_capacity
      procedure, public, non_overridable :: clear => i64_array_clear
  end type

  interface assignment(=)
    module procedure :: assign_i64array_array
  end interface assignment(=)

 !> Assign a character sequence to a string.
    interface assignment(=)
        module procedure :: assign_string_char
    end interface assignment(=)
contains

  function split(self)
    class(String), intent(in) :: self
    type(String), allocatable :: split(:)
    type(String) :: new_str
    integer(i64) :: i,self_end,word_start

    allocate(split(0))

    i = 1
    word_start = 1
    self_end = self%length()

    do 
      ! Find the start of the next word
      do while (self%buf(i:i) == ' ')
        if (i == self_end) then
          exit
        end if
        i = i+1
      end do

      word_start = i

      ! Find the end of this word
      do while(self%buf(i:i) /= ' ')
        if (i == self_end) then
          exit
        end if
        i = i+1
      end do

      new_str = self%buf(word_start:i)

      split = [ split, new_str ]

      if (i == self_end) then
        exit
      end if
      
    end do

  end function

  function i64_array_array(self) result(o)
    class(i64Array), target, intent(inout) :: self
    integer(i64), pointer :: o(:)

    o => self%buf(:self%n)
  end function

  elemental impure subroutine i64_array_clear(self)
    class(i64Array), intent(inout) :: self
      self%n = 0
  end subroutine

  function i64_array_back(self) result(o)
    class(i64Array), target, intent(inout) :: self
    integer(i64), pointer :: o
    if (self%n == 0) then
      write(*,*) "Char Array is size 0!"
      stop
    endif
    o => self%buf(self%n)
  end function

  function i64_array_back_many(self, m) result(o)
    class(i64Array), intent(in) :: self
    integer(i64), intent(in) :: m
    integer(i64), allocatable :: o(:)
    if (self%n < m) then
      write(*,*) "Char Array is too small!"
      stop
    endif
    o = self%buf(self%n-m+1:self%n)
  end function

  integer(i64) function i64_array_size(self) result(o)
    class(i64Array), intent(in) :: self
    o = self%n
  end function

  integer(i64) function i64_array_capacity(self) result(o)
    class(i64Array), intent(in) :: self
    if (.not. allocated(self%buf)) then
      o = 0
    else
      o = size(self%buf)
    end if
  end function

  logical function i64_array_pop(self) result(o)
    class(i64Array), intent(inout) :: self
    if (self%n /= 0) then
      self%n = self%n - 1
      o = .true.
    else
      o = .false.
    end if
  end function

  logical function i64_array_pop_many(self, m) result(o)
    class(i64Array), intent(inout) :: self
    integer(i64), intent(in) :: m
    if (self%n >= m) then
      self%n = self%n - m
      o = .true.
    else
      o = .false.
    end if
  end function

  subroutine i64_array_push(self, c)
    class(i64Array),intent(inout) :: self
    integer(i64), intent(in) :: c

    if (.not. allocated(self%buf)) then
      allocate(self%buf(1))
    end if

    if (self%n == self%capacity()) then
      block
        integer(i64), allocatable :: new_buf(:)
        integer(i64) :: i, new_cap

        new_cap = int(real(self%capacity(),r64) * grow, i64) + 1
        allocate(new_buf(new_cap))

        do i=1,self%n
          new_buf(i) = self%buf(i)
        end do

        call move_alloc(new_buf,self%buf)
      end block
    end if

    self%n = self%n+1
    self%buf(self%n) = c
  end subroutine

  subroutine i64_array_push_many(self, c)
    class(i64Array),intent(inout) :: self
    integer(i64), intent(in) :: c(:)
    integer(i64) :: m

    m = size(c)

    if (.not. allocated(self%buf)) then
      allocate(self%buf(m))
    end if

    if (self%n + m >= self%capacity()) then
      block
        integer(i64), allocatable :: new_buf(:)
        integer(i64) :: i, new_cap

        new_cap = self%n + m
        allocate(new_buf(new_cap))

        new_buf(:self%n) = self%buf(:self%n)

        call move_alloc(new_buf,self%buf)
      end block
    end if

    self%buf(self%n+1:self%n+m) = c(:)
    self%n = self%n + m
  end subroutine

  elemental impure subroutine char_array_clear(self)
    class(CharArray), intent(inout) :: self
      self%n = 0
  end subroutine

  character elemental impure function char_array_back(self) result(o)
    class(CharArray), intent(in) :: self
    if (self%n == 0) then
      write(*,*) "Char Array is size 0!"
      stop
    endif
    o = self%buf(self%n)
  end function

  function char_array_back_many(self, m) result(o)
    class(CharArray), intent(in) :: self
    integer(i64), intent(in) :: m
    character, allocatable :: o(:)
    if (self%n < m) then
      write(*,*) "Char Array is too small!"
      stop
    endif
    o = self%buf(self%n-m+1:self%n)
  end function

  integer(i64) function char_array_size(self) result(o)
    class(CharArray), intent(in) :: self
    o = self%n
  end function

  integer(i64) function char_array_capacity(self) result(o)
    class(CharArray), intent(in) :: self
    if (.not. allocated(self%buf)) then
      o = 0
    else
      o = size(self%buf)
    end if
  end function

  logical function char_array_pop(self) result(o)
    class(CharArray), intent(inout) :: self
    if (self%n /= 0) then
      self%n = self%n - 1
      o = .true.
    else
      o = .false.
    end if
  end function

  logical function char_array_pop_many(self, m) result(o)
    class(CharArray), intent(inout) :: self
    integer(i64), intent(in) :: m
    if (self%n >= m) then
      self%n = self%n - m
      o = .true.
    else
      o = .false.
    end if
  end function

  subroutine char_array_push(self, c)
    class(CharArray),intent(inout) :: self
    character, intent(in) :: c

    if (.not. allocated(self%buf)) then
      allocate(self%buf(1))
    end if

    if (self%n == self%capacity()) then
      block
        character, allocatable :: new_buf(:)
        integer(i64) :: i, new_cap

        new_cap = int(real(self%capacity(),r64) * grow,1_i64) + 1
        allocate(new_buf(new_cap))

        do i=1,self%n
          new_buf(i) = self%buf(i)
        end do

        call move_alloc(new_buf,self%buf)
      end block
    end if

    self%n = self%n+1
    self%buf(self%n) = c
  end subroutine

  subroutine char_array_push_many(self, c)
    class(CharArray),intent(inout) :: self
    character, intent(in) :: c(:)
    integer(i64) :: m

    m = size(c)

    if (.not. allocated(self%buf)) then
      allocate(self%buf(m))
    end if

    if (self%n + m >= self%capacity()) then
      block
        character, allocatable :: new_buf(:)
        integer(i64) :: i, new_cap

        new_cap = self%n + m
        allocate(new_buf(new_cap))

        new_buf(:self%n) = self%buf(:self%n)

        call move_alloc(new_buf,self%buf)
      end block
    end if

    self%buf(self%n+1:self%n+m) = c(:)
    self%n = self%n + m
  end subroutine

  subroutine assign_i64array_array(lhs, rhs)
      type(i64Array), intent(inout) :: lhs
      integer(i64), intent(in) :: rhs(:)
      lhs%buf = rhs
      lhs%n = size(rhs)
  end subroutine 

  elemental subroutine assign_string_char(lhs, rhs)
      type(String), intent(inout) :: lhs
      character(len=*), intent(in) :: rhs
      lhs%buf = rhs
  end subroutine 

  logical function is_upper(c) 
    character,intent(in) :: c
    is_upper = (c >= 'A' .and. c <= 'Z')
  end function

  subroutine assert(cond)
    logical, intent(in) :: cond
    if (.not. cond) then
      stop "Assertion failed"
    end if
  end subroutine

  subroutine replace(self, a,b)
    class(String), intent(inout) :: self
    character, intent(in) :: a, b
    integer :: i

    do i=1,len(self%buf)
      if (self%buf(i:i) == a) self%buf(i:i) = b
    end do
  end subroutine

  function readlines(self) result(lines)
    class(FormattedFile), intent(inout) :: self
    type(string), allocatable :: lines(:)

    allocate(lines(0))

    do while (self%scanline()) 
      lines = [lines,self%text()]
    end do
  end function

  logical function scanline(self) result(r)
    class(FormattedFile), intent(inout) :: self
    integer :: stat,chunk
    character(4096) :: line
    character(1024) :: err_msg

    stat = 0
    
    self%buf = ""

    do while (stat == 0)
      read(self%funit,'(A)', advance="no", iostat=stat, iomsg=err_msg, size=chunk) line
      self%buf = self%buf // line(:chunk)
    end do

    if (stat == iostat_eor) then
      r = .true.
    else if (stat == iostat_end) then
      r = .false.
    else 
      write(*,*) "Error reading file"
      stop
    end if

  end function

  type(String) function text(self) result(s)
    class(FormattedFile), intent(inout) :: self
    call move_alloc(self%buf, s%buf)
  end function

  integer function length(self) result(l)
    class(String), intent(in) :: self
    l = len(self%buf)
  end function

  integer(i64) elemental impure function to_i64(self) result(i)
    class(String), intent(in) :: self
    integer :: stat
    character(1024) :: msg

    read(self%buf, *, iostat=stat, iomsg=msg) i

    if (stat /= 0) then
      write(*,*) msg
      stop
    end if
  end function

  type(FormattedFile) function fopen(fname)
    character(*), intent(in) :: fname
    integer :: stat
    character(1024) :: err_msg

    open(newunit=fopen%funit, file=fname, form="formatted", action="read", pad="yes", iomsg=err_msg, iostat=stat)

    if (stat /= 0) then
      write(*,*) err_msg
      stop
    end if

  end function

end module

