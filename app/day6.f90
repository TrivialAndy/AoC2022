program day6
   use, intrinsic :: iso_fortran_env, only: iostat_eor
   use utils, only: get_input
   use day6_utils, only: unique
   implicit none

   character(len=80) :: filename
      !! The input file
   integer :: handle
      !! The unit to read from

   integer :: n
      !! The number of chars read
   character(len=14) :: queue
      !! The possibly unique substring
   integer :: head
      !! The next value in queue to update

   integer :: stat
      !! Result from iostat
   character(len=80) :: msg

   call get_input(filename, handle)
   n = 4
   head = 1
   read (handle, '(a4)', advance='no') queue
   do while (.not. unique(queue))
      read (handle, '(a1)', iostat=stat, iomsg=msg, advance='no') queue(head:head)
      if (stat /= 0) then
         print *, msg, stat
         exit
      end if
      if (head == len(queue)) head = 0
      head = head + 1
      n = n + 1
   end do

   print *, "First marker after ", n, " chars"
end program day6
