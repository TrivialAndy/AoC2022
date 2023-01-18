program day2
   use, intrinsic :: iso_fortran_env, only: iostat_end
   use utils, only: get_input
   use day2_utils, only: calc_round, fix_round
   implicit none

   character(len=50) :: filename
      !! The name of the problem
   integer :: handle
      !! The unit to read input from

   character(len=3) :: round
      !! The input for the round
   integer :: stat
      !! IO stat of a read

   integer :: total_score
   !! The total score so far
   integer :: total_score2
   !! The total score so far

   call get_input(filename, handle)
   total_score = 0
   total_score2 = 0

   read (handle, '(a3)', iostat=stat) round
   ! print *, round
   do while (stat /= iostat_end)
      total_score = total_score + calc_round(round(1:1), round(3:3))
      total_score2 = total_score2 + fix_round(round(1:1), round(3:3))
      read (handle, '(a3)', iostat=stat) round
   end do

   print *, "Total score will be (choose rps)", total_score
   print *, "Total score will be (choose outcome)", total_score2

end program day2
