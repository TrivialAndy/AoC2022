program day5
   use, intrinsic :: iso_fortran_env, only: iostat_end
   use day5_utils, only: read_stacks, parse_move, Stack
   use utils, only: get_input
   implicit none

   character(len=80) :: filename
      !! The filename chosen
   integer :: handle
      !! The unit to use for reads

   type(Stack), allocatable, dimension(:) :: stacks
      !! The stacks to manipulate

   integer :: number
      !! Number of crates to move
   integer :: from
      !! Which stack to take crates from
   integer :: to
      !! Which stack to put crates on

   integer :: stat
      !! iostat from read

   integer :: i
      !! Loop counter
   character, allocatable, dimension(:) :: tmpn
      !! Temp value storage
   character :: tmp
      !! Temp value storage

   call get_input(filename, handle)
   call read_stacks(handle, stacks)

   stat = 0

   do while (stat /= iostat_end)
      call parse_move(handle, number, from, to, stat)
      if (stat /= 0) exit
      call stacks(from)%popn(number, tmpn)
      call stacks(to)%pushn(tmpn)
      deallocate (tmpn)
   end do
   do i = 1, size(stacks)
      call stacks(i)%pop(tmp)
      write (*, '(a1)', advance="no"), tmp
   end do
   print *, " "

end program day5
