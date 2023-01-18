program day4
   use, intrinsic :: iso_fortran_env, only: iostat_end
   use utils, only: get_input
   use day4_utils, only: read_assignments, eclipses, overlaps
   implicit none

   character(len=80) :: filename
      !! The filename of the problem input
   integer :: handle
      !! The unit to read input from

   character(len=80) :: assignments
      !! A single line of assignments
   integer, dimension(2) :: r1, r2
      !! Elf assignment ranges (start, stop)

   integer :: total_e
      !! Number of times elf schedules were eclipsed
   integer :: total_o
      !! Number of times elf schedules overlaped

   integer :: stat
      !! Storage for iostat

   total_e = 0
   total_o = 0

   call get_input(filename, handle)
   read(handle, '(a80)', iostat=stat) assignments

   do while (stat /= iostat_end)
      call read_assignments(assignments, r1, r2)
      if (eclipses(r1,r2)) total_e = total_e + 1
      if (overlaps(r1,r2)) total_o = total_o + 1
      read(handle, '(a80)', iostat=stat) assignments
   end do

   print *, "Elves that were eclipsed:", total_e
   print *, "Elves that overlap:", total_o

end program day4