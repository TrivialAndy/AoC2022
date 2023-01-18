module day1_utils
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none
   private
   public :: get_calories, update_richest
contains

   subroutine get_calories(file, index, calories, eof)
      integer, intent(in) :: file
         !! The unit to read from
      integer, intent(inout) :: index
         !! The index of the last read elf
      integer, intent(out) :: calories
         !! The number of calories the elf has
      integer, intent(out) :: eof
         !! 1 if the end of file has been reached

      character(len=20) :: line
         !! The next line from the file
      integer :: snack
         !! The calorie value of the next snack to consider

      integer :: error
         !! Error from read
      character(len=20) :: msg
         !! Error message

      index = index + 1
      calories = 0

      read (file, *, iostat=error) line
      if (error == iostat_end) then
         eof = 1
         calories = -1
         return
      end if

      do while (line /= " " .and. error /= iostat_end)
         read (line, '(I20)') snack
         calories = calories + snack
         read (file, '(a20)', iostat=error, iomsg=msg) line
      end do

      if (error == iostat_end) eof = 1

   end subroutine get_calories

   subroutine update_richest(calories, indices, new_calorie, new_index)
      integer, dimension(3), intent(inout) :: calories
         !! The current richest elves calories
      integer, dimension(3), intent(inout) :: indices
         !! The current richest elves indices
      integer, intent(in) :: new_calorie
         !! The next elf's calories
      integer, intent(in) :: new_index
         !! The next elf's index

      integer :: working_calorie
         !! Working value for calories
      integer :: working_index
         !! The working value for index

      integer :: i
         !! Loop counter
      integer :: temp
         !! Temp value

      working_calorie = new_calorie
      working_index = new_index
      do i = 1, 3
         if (working_calorie > calories(i)) then
            temp = working_calorie
            working_calorie = calories(i)
            calories(i) = temp

            temp = working_index
            working_index = indices(i)
            indices(i) = temp
         end if
      end do
   end subroutine update_richest

end module day1_utils
