module day9_utils
   implicit none
   private
   public :: Rope

   type :: DynamicArray2Int
      integer :: n
         !! Number of items in array
      integer, allocatable, dimension(:, :) :: arr
         !! The values of the array
   contains
      procedure :: append
   end type DynamicArray2Int

   type :: Rope
      integer :: length
         !! Max distance of tail from head
      integer, dimension(2) :: head
         !! Head of the rope
      integer, dimension(2) :: tail
         !! Tail of the rope
      integer, dimension(4) :: extent
         !! Min and max x and y positions for tail
      type(DynamicArray2Int) :: history
         !! Positions where tail has been
   contains
      procedure :: move
      procedure :: tail_history
   end type Rope
contains
   subroutine append(self, val)
      !! Add a value to the subroutine
      class(DynamicArray2Int), intent(inout) :: self
         !! The array to update
      integer, dimension(2), intent(in) :: val
         !! The value to add

      integer, allocatable, dimension(:, :) :: tmp
         !! Temp array for reallocating arr

      if (.not. allocated(self%arr)) then
         self%n = 0
         allocate (self%arr(10, 2))
      end if

      if (self%n == size(self%arr, 1)) then
         allocate (tmp(self%n, 2))
         tmp = self%arr
         deallocate (self%arr)
         allocate (self%arr(self%n*2, 2))
         self%arr(1:self%n, :) = tmp
         deallocate (tmp)
      end if
      self%n = self%n + 1
      self%arr(self%n, :) = val
   end subroutine

   subroutine move(self, delta)
      class(Rope), intent(inout) :: self
         !! The rope to move
      integer, dimension(2), intent(in) :: delta
         !! The move to make

      integer, dimension(2) :: gap
         !! Gap between head and tail

      self%head = self%head + delta
      gap = self%head - self%tail
      do while (maxval(abs(gap)) > self%length)
         if (gap(1) /= 0) self%tail(1) = self%tail(1) + gap(1)/abs(gap(1))
         if (gap(2) /= 0) self%tail(2) = self%tail(2) + gap(2)/abs(gap(2))
         if (allocated(self%history%arr)) call self%history%append(self%tail)
         gap = self%head - self%tail
      end do
      if (self%tail(1) < self%extent(1)) self%extent(1) = self%tail(1)
      if (self%tail(1) > self%extent(2)) self%extent(2) = self%tail(1)
      if (self%tail(2) < self%extent(3)) self%extent(3) = self%tail(2)
      if (self%tail(2) > self%extent(4)) self%extent(4) = self%tail(2)
   end subroutine move

   subroutine tail_history(self, unique_pos)
      !! Calculate the number of unique positions for the tail
      class(Rope), intent(in) :: self
         !! The rope to check
      integer, intent(out) :: unique_pos
         !! Number of unique positions

      integer, allocatable, dimension(:, :) :: hashmap
         !! A grid to map tail on

      integer :: i
         !! Loop counter

      if (.not. allocated(self%history%arr)) then
         unique_pos = 1
         return
      end if

      allocate (hashmap(self%extent(1):self%extent(2), &
                        self%extent(3):self%extent(4)))

      do i = 1, self%history%n
         hashmap(self%history%arr(i, 1), self%history%arr(i, 2)) = 1
      end do

      unique_pos = sum(hashmap)
      deallocate (hashmap)
   end subroutine tail_history

end module day9_utils
