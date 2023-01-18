program day8
   use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
   use utils, only: get_input
   use day8_utils, only: map_visible, scenic_scores
   implicit none

   character(len=80) :: filename
      !! Problem filename
   integer :: handle
      !! The unit to read from

   integer, allocatable, dimension(:, :) :: trees
      !! The tree heights
   integer, allocatable, dimension(:, :) :: visible
      !! Visible trees

   integer :: x, y
      !! The array extents in x and y respectively

   character(len=10) :: batch
      !! A single batch of input from a line
   integer :: batch_id
      !! Counter for reading data
   integer :: i, j
      !! Loop counter
   integer :: stat
      !! iostat return value
   character(len=80) :: msg
      !! iomsg return value

   call get_input(filename, handle)

   read (handle, '(a10)', advance='no', iostat=stat, iomsg=msg) batch
   ! Determine extents
   x = 0
   ! print *, stat, msg
   ! print *, batch
   do while (stat == 0)
      ! print *, batch
      x = x + 10
      read (handle, '(a10)', advance='no', iostat=stat) batch
   end do
   x = x + len_trim(batch)
   y = 1
   read (handle, '(a10)', iostat=stat) batch
   do while (stat == 0)
      y = y + 1
      read (handle, '(a10)', iostat=stat) batch
   end do

   print *, 'X = ', x, ', y = ', y

   ! Read in array
   allocate (trees(x, y))
   rewind (handle)
   do i = 1, y
      do batch_id = 1, (x/10)
         read (handle, '(a10)', advance='no') batch
         do j = 1, 10
            read (batch(j:j), *) trees((batch_id - 1)*10 + j, i)
         end do
      end do
      read (handle, '(a10)') batch
      do j = 1, len_trim(batch)
         read (batch(j:j), *) trees(x - len_trim(batch) + j, i)
      end do
   end do

   call map_visible(trees, visible)
   print *, "I can see ", sum(visible), "trees"

   call scenic_scores(trees, visible)
   print *, "The best score is ", maxval(visible)
   ! do i = 1, y
   !    do j = 1, x
   !       write (*, '(i1)', advance='no') visible(j, i)
   !    end do
   !    write (*, *) ' '
   ! end do

end program day8
