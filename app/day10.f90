program day10
   use utils, only: get_input
   use day10_utils, only: Comp
   implicit none

   character(len=80) :: filename
      !! The filename for the problem
   integer :: handle
      !! The unit to read from

   type(Comp) :: computer
      !! The computer to simulate

   character(len=80) :: line
      !! A single line of input
   integer :: arg
      !! Temp int for reading args

   integer :: stat
      !! iostat return value

   call get_input(filename, handle)

   call computer%init()
   read (handle, '(a80)', iostat=stat) line
   do while (stat == 0)
      if (line(1:4) == 'noop') then
         call computer%noop()
      else if (line(1:4) == 'addx') then
         read (line(6:80), *) arg
         call computer%addx(arg)
      end if
      read (handle, '(a80)', iostat=stat) line
   end do

   print *, "Total is ", computer%tot_strength

end program day10
