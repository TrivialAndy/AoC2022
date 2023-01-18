module day4_utils
   implicit none
   private
   public:: read_assignments, eclipses, parse_assignment, overlaps
contains
   subroutine read_assignments(assignments, r1, r2)
      !! Parse assignment strings into integer pairs
      character(len=*), intent(in) :: assignments
         !! The assignments as a string
      integer, dimension(2), intent(out) :: r1, r2
         !! Ranges for each elf (start, stop)

      character(len=20) :: tmp1, tmp2
         !! Temp strings to read in to

      read (assignments, *) tmp1, tmp2
      call parse_assignment(tmp1, r1)
      call parse_assignment(tmp2, r2)

   end subroutine read_assignments

   subroutine parse_assignment(str, range)
      !! Parse a single elf assignment
      character(len=*), intent(in) :: str
         !! The string to parse
      integer, dimension(2), intent(out) :: range
         !! The range to return

      integer :: i
         !! Index

      i = index(str, "-")
      ! print *, str(1:i-1), '...',str(i + 1:len(str))
      read (str(1:i-1), *) range(1)
      read (str(i + 1:len(str)), *) range(2)

   end subroutine parse_assignment

   function eclipses(r1, r2) result(retval)
      !! Check if either range eclipse the other
      integer, dimension(2), intent(in) :: r1, r2
         !! The ranges to check
      logical :: retval
         !! True if one range is eclipsed

      retval = .false.
      if (r1(1) <= r2(1) .and. r1(2) >= r2(2)) retval = .true.
      if (r1(1) >= r2(1) .and. r1(2) <= r2(2)) retval = .true.

   end function eclipses

   function overlaps(r1, r2) result(retval)
      !! Check if either range eclipse the other
      integer, dimension(2), intent(in) :: r1, r2
         !! The ranges to check
      logical :: retval
         !! True if ranges overlap

      
      retval = .false.
      if (r1(1) <= r2(1) .and. r1(2) >= r2(1)) retval = .true.
      if (r1(1) >= r2(1) .and. r1(2) <= r2(2)) retval = .true.
      if (r1(1) <= r2(2) .and. r1(2) >= r2(2)) retval = .true.
   
      
   end function overlaps
end module day4_utils
