module aoc_utils
  use iso_fortran_env
  implicit none

  type :: FormattedFile
    private
    integer :: funit
    character(:), allocatable :: buf

    contains
      procedure, public, non_overridable :: scanline
      procedure, public, non_overridable :: text
  end type

  type :: String
    character(:), allocatable :: buf
    contains
      procedure, public, non_overridable :: length
      procedure, public, non_overridable :: to_int64
  end type

contains

  logical function scanline(self) result(r)
    class(FormattedFile), intent(inout) :: self
    integer :: stat,chunk
    character(4096) :: line
    character(1024) :: err_msg

    stat = 0
    
    if (allocated(self%buf)) then
      deallocate(self%buf)
    end if

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

  integer(int64) function length(self) result(l)
    class(String), intent(in) :: self
    l = len(self%buf, 1_int64)
  end function

  integer(int64) function to_int64(self) result(i)
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

