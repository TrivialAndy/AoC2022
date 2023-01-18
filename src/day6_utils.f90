module day6_utils
   implicit none
   private
   public :: unique
contains
   function unique(queue) result(retval)
      !! Check if a string is filled with unique chars
      character(len=*), intent(in) :: queue
         !! The string to check
      logical :: retval
         !! Return value

      integer :: i, j
         !! Loop counters

      retval = .true.
      do i = 1, len(queue)
         do j = (i + 1), len(queue)
            if (queue(i:i) == queue(j:j)) then
               retval = .false.
               exit
            end if
         end do
         if (.not. retval) exit
      end do
   end function unique
end module day6_utils
