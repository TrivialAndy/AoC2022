program day3
   use utils, only: get_input
   use day3_utils, only: convert_backpack, get_duplicate
   use, intrinsic :: iso_fortran_env, only: iostat_end
   implicit none

   character(len=50) :: filename
      !! The problem name
   integer :: handle
      !! The file unit to read from

   character(len=80) :: backpack
      !! A single backpack
   integer, allocatable, dimension(:) :: bp1, bp2, bp3, dummy
      !! Backpack scores

   integer :: total
      !! The sum of the duplicates

   integer :: stat
      !! Value for iostat

   call get_input(filename, handle)

   read (handle, '(a80)', iostat=stat) backpack
   do while (stat /= iostat_end)
      call convert_backpack(backpack, bp1, bp2, split=.true.)
      total = total + get_duplicate(bp1, bp2)
      read (handle, '(a80)', iostat=stat) backpack
   end do
   ! do while (stat /= iostat_end)
   !    call convert_backpack(backpack, bp1, dummy)
   !    read (handle, '(a80)', iostat=stat) backpack
   !    call convert_backpack(backpack, bp2, dummy)
   !    read (handle, '(a80)', iostat=stat) backpack
   !    call convert_backpack(backpack, bp3, dummy)
   !    read (handle, '(a80)', iostat=stat) backpack
   !    total = total + get_duplicate(bp1, bp2, bp3)
   !    deallocate(bp1)
   !    deallocate(bp2)
   !    deallocate(bp3)
   ! end do

   print *, "Sum of duplicates is", total

end program day3
