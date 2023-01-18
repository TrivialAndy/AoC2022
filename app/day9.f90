program day9
   use utils, only: get_input
   use day9_utils, only: Rope
   implicit none

   character(len=80) :: filename
      !! The name of the problem file
   integer :: handle
      !! The unit to read from

   type(Rope), dimension(9) :: r
      !! The rope to model

   character(len=80) :: line
      !! One line of input
   integer :: stat
      !! Result from iostat

   integer, dimension(2) :: delta
      !! The amount to move the rope
   integer :: arg
      !! Temp int for move arguments

   integer :: i, j
      !! Loop counter

   do i = 1, 9
      r(i)%head = (/1, 1/)
      r(i)%tail = r(i)%head
      r(i)%extent = (/1, 1, 1, 1/)
      r(i)%length = 1
   end do

   ! Enable tail tracking on final tail only
   call r(9)%history%append(r(9)%tail)

   call get_input(filename, handle)

   read (handle, '(a80)', iostat=stat) line
   do while (stat == 0)
      read (line(3:80), *) arg
      do i = 1, arg
         delta = (/0, 0/)
         if (line(1:1) == 'R') delta(1) = 1
         if (line(1:1) == 'L') delta(1) = -1
         if (line(1:1) == 'U') delta(2) = 1
         if (line(1:1) == 'D') delta(2) = -1
         call r(1)%move(delta)
         do j = 2, 9
            delta = r(j - 1)%tail - r(j)%head
            call r(j)%move(delta)
         end do
      end do
      read (handle, '(a80)', iostat=stat) line
   end do

   call r(9)%tail_history(arg)
   print *, "Vitited ", arg, " spots"

end program day9
