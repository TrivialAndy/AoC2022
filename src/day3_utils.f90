module day3_utils
   use stdlib_sorting, only: sort
   implicit none
   private
   public :: convert_backpack, get_duplicate, convert_char
contains
   function convert_char(c) result(s)
      !! Convert a single char to a score

      character, intent(in) :: c
         !! Character to convert
      integer :: s
         !! Score

      s = iachar(c)

      if ('a' <= c .and. c <= 'z') s = s - iachar('a') + 1
      if ('A' <= c .and. c <= 'Z') s = s - iachar('A') + 27
   end function convert_char

   subroutine convert_backpack(backpack, scores1, scores2, split)
      !! Convert the backpack to integers
      character(len=*), intent(in) :: backpack
         !! The input backpack
      integer, allocatable, dimension(:), intent(out) :: scores1
         !! The scores of each character (left pouch if split is true)
      integer, allocatable, dimension(:), intent(out) :: scores2
         !! The scores of right pouch (only allocated if split)
      logical, intent(in), optional :: split
         !! Split the data

      integer :: n
         !! Number of items in the backpack
      logical :: asplit
         !! Actual value for split (non-optional)

      integer :: i
         !! Loop counter

      n = len_trim(backpack)
      if (present(split)) then
         asplit = split
      else
         asplit = .false.
      end if
      if (asplit) then
         allocate (scores1(n/2))
         allocate (scores2(n/2))
         do i = 1, n/2
            scores1(i) = convert_char(backpack(i:i))
            scores2(i) = convert_char(backpack(i + n/2:i + n/2))
         end do
         call sort(scores1)
         call sort(scores2)
      else
         allocate (scores1(n))
         do i = 1, n
            scores1(i) = convert_char(backpack(i:i))
         end do
         call sort(scores1)
      end if

   end subroutine convert_backpack

   function get_duplicate(scores1, scores2, scores3) result(dup)
      !! Find the duplicate entry
      integer, allocatable, dimension(:), intent(in) :: scores1
      !! The scores for bp 1
      integer, allocatable, dimension(:), intent(in) :: scores2
      !! The scores for bp 2
      integer, allocatable, dimension(:), intent(in), optional :: scores3
      !! The scores for bp 3

      integer :: dup
         !! The duplicated score

      integer :: i, j, k
         !! The working indices
      i = 1
      j = 1
      k = 1

      dup = 0
      do while (dup == 0)
         if (i > size(scores1) .or. j > size(scores2)) then
            dup = -1
         else if (scores1(i) < scores2(j)) then
            i = i + 1
         else if (scores1(i) > scores2(j)) then
            j = j + 1
         else if (present(scores3)) then
            if (k > size(scores3)) then
               dup = -1
            else if (scores1(i) < scores3(k)) then
               i = i + 1
               j = j + 1
            else if (scores1(i) > scores3(k)) then
               k = k + 1
            else
               dup = scores1(i)
            end if
         else
            dup = scores1(i)
         end if
      end do
      ! print *, "Dup: ", dup

   end function get_duplicate
end module day3_utils
