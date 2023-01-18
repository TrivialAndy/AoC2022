module utils
   implicit none
   private

   public :: get_input
contains
   subroutine get_input(filename, handle)
      character(len=50), intent(out) :: filename
         !! Filename to run with (arg1)
      integer, intent(out) :: handle
         !! Unit for the file

      if (command_argument_count() < 1) then
         print *, "Usage: Must be passed a filename."
         return
      end if
      call get_command_argument(1, filename)
   
      handle = 20
      open (handle, file=filename)

   end subroutine get_input
end module utils
