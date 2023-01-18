module day8_utils
   implicit none
   private
   public :: map_visible, scenic_scores
contains
   subroutine map_visible(arr, mask)
      !! Create a mask where 1 indicates visible and 0 hidden
      integer, dimension(:, :), intent(in) :: arr
         !! The array to mask
      integer, allocatable, dimension(:, :), intent(out) ::  mask
         !! Return mask

      integer, allocatable, dimension(:) :: highest
         !! Array for the highest value seen

      integer :: x, y
         !! Extent of the array in x and y

      integer :: i, j
         !! Loop counter

      integer::idx

      x = size(arr, 1)
      y = size(arr, 2)
      allocate (mask(x, y))
      mask = 0

      ! Top to bottom
      allocate (highest(x))
      highest = -1
      do i = 1, y
         do j = 1, x
            if (arr(j, i) > highest(j)) then
               mask(j, i) = 1
               highest(j) = arr(j, i)
            end if
         end do
      end do
      ! Bottom to top
      highest = -1
      do i = 1, y
         idx = y - i + 1
         do j = 1, x
            if (arr(j, idx) > highest(j)) then
               mask(j, idx) = 1
               highest(j) = arr(j, idx)
            end if
         end do
      end do
      deallocate (highest)
      ! Left to right
      allocate (highest(y))
      highest = -1
      do i = 1, x
         do j = 1, y
            if (arr(i, j) > highest(j)) then
               mask(i, j) = 1
               highest(j) = arr(i, j)
            end if
         end do
      end do
      ! Right to left
      highest = -1
      do i = 1, x
         idx = x + 1 - i
         do j = 1, y
            if (arr(idx, j) > highest(j)) then
               mask(idx, j) = 1
               highest(j) = arr(idx, j)
            end if
         end do
      end do
      deallocate (highest)
   end subroutine map_visible

   subroutine scenic_scores(arr, res)
      integer, dimension(:, :), intent(in) :: arr
      integer, dimension(:, :), allocatable, intent(out) :: res

      integer :: x, y
         !! Extent of the array in x and y

      integer :: i, j, k
         !! Loop counter

      integer :: vis

      x = size(arr, 1)
      y = size(arr, 2)

      if (allocated(res)) deallocate (res)
      allocate (res(x, y))
      res = 1
      res(1, :) = 0
      res(x, :) = 0
      res(:, 1) = 0
      res(:, y) = 0

      do i = 2, x - 1
         do j = 2, y - 1
            ! Left to right
            vis = 0
            k = 0
            do while (.true.)
               k = k + 1
               vis = vis + 1
               if (i - k == 1 .or. arr(i - k, j) >= arr(i, j)) exit
            end do
            res(i, j) = res(i, j)*vis
            ! Right to left
            vis = 0
            k = 0
            do while (.true.)
               k = k + 1
               vis = vis + 1
               if (i + k == x .or. arr(i + k, j) >= arr(i, j)) exit
            end do
            res(i, j) = res(i, j)*vis
            ! Bottom to top
            vis = 0
            k = 0
            do while (.true.)
               k = k + 1
               vis = vis + 1
               if (j - k == 1 .or. arr(i, j - k) >= arr(i, j)) exit
            end do
            res(i, j) = res(i, j)*vis
            ! Top to bottom
            vis = 0
            k = 0
            do while (.true.)
               k = k + 1
               vis = vis + 1
               if (j + k == y .or. arr(i, j + k) >= arr(i, j)) exit
            end do
            res(i, j) = res(i, j)*vis
         end do
      end do
   end subroutine scenic_scores
end module day8_utils
