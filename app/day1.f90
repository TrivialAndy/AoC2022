program day1
   use day1_utils, only: get_calories, update_richest
   implicit none

   character(len=50) :: filename
      !! Filename to run with (arg1)

   integer, dimension(3) :: max_calories
      !! The current maximum calories
   integer, dimension(3) :: richest_elves
      !! The elf with the highest calorie snacks

   integer :: calories
      !! Calories of next elf to consider
   integer :: index
      !! Index of next elf to consider

   integer :: eof
      !! If the parsing has reached the end of file

   if (command_argument_count() /= 1) then
      print *, "Usage: Only argument must be filename."
      stop
   end if
   call get_command_argument(1, filename)

   eof = 0

   index = 0
   open (20, file=filename)

   max_calories = (/-1,-1,-1/)
   do while (eof == 0)
      call get_calories(20, index, calories, eof)
      call update_richest(max_calories, richest_elves, calories, index)
   end do

   calories = 0
   do index = 1, 3
      print *, index, " richest elf is", richest_elves(index), "with", &
         max_calories(index), "calories"
      calories = calories + max_calories(index)
   end do
   print *, "Total: ", calories
end program day1
