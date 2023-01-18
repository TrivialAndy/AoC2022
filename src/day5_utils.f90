module day5_utils
   implicit none
   private
   public :: read_stacks, parse_move, Stack

   type :: Stack
      !! Implement a simple stack
      character, allocatable, dimension(:) :: s
         !! The stack list
      integer :: head
         !! The current head
   contains
      procedure :: push
         !! Add a single value to the stack
      procedure :: pop
         !! Remove a single value from a stack
      procedure :: pushn
         !! Push a list onto the stack
      procedure :: popn
         !! Pop a list of the stack
   end type

contains
   subroutine push(self, val)
      !! Push a single value onto the stack
      class(Stack), intent(inout) :: self
         !! The stack
      character, intent(in) :: val
         !! Value to push

      character, dimension(1) :: val_arr
         !! Temp array for value

      val_arr(1) = val
      call self%pushn(val_arr)
   end subroutine push

   subroutine pop(self, val)
      !! Pop a single value off the stack
      class(Stack), intent(inout) :: self
         !! The stack
      character, intent(out) :: val
         !! Output value

      character, allocatable, dimension(:) :: val_arr
         !! Temp storage for value

      call self%popn(1, val_arr)
      val = val_arr(1)
   end subroutine pop

   subroutine pushn(self, val)
      !! Push values onto the stack maintaining order
      class(Stack), intent(inout) :: self
         !! The object representing the stack
      character, dimension(:), intent(in) :: val
         !! The values to add

      character, allocatable, dimension(:) :: work
         !! Used for reallocating stack to grow it
      integer :: n
         !! Number of elements in array
      integer :: new_n
         !! New number of elements in array

      integer :: i
         !! Loop counter

      if (.not. allocated(self%s)) then
         self%head = 0
         allocate (self%s(10))
      end if

      ! print*, "Pushing ", val
      n = self%head
      self%head = n + size(val)
      if (self%head > size(self%s)) then
         print *, "growing from ", n, " to ", self%head
         allocate (work(n))
         work = self%s
         deallocate (self%s)
         new_n = n*2 + 1
         do while (new_n < self%head)
            new_n = new_n*2
         end do
         allocate (self%s(new_n))
         do i = 1, n
            self%s(i) = work(i)
         end do
         deallocate (work)
      end if
      do i = 1, size(val)
         self%s(n + i) = val(i)
         print *, "Pushing ", val(i)
      end do
      print *, " "
   end subroutine pushn

   subroutine popn(self, n, val)
      !! Pop n items from the stack maintaining order
      class(Stack), intent(inout) :: self
         !! The object representing a stack
      integer, intent(in) :: n
         !! Number of values to pop
      character, allocatable, dimension(:), intent(out) :: val
         !! The values popped from the stack

      integer :: i
         !! Loop counter

      if (.not. allocated(self%s) .or. self%head == 0) return

      allocate (val(n))
      do i = 1, n
         val(i) = self%s(self%head - n + i)
      end do
      self%head = self%head - n
   end subroutine popn

   subroutine read_stacks(handle, stacks)
      integer, intent(in) :: handle
         !! Unit to read the stacks from
      type(Stack), allocatable, dimension(:), intent(out) :: stacks
         !! The stacks read in

      character(len=80) :: line
         !! One line from the file

      integer :: n
         !! Number of stacks
      character :: tmp
         !! Temporary var for values

      integer :: line_no
         !! Bookmark for rewinding through file
      integer :: i
         !! Loop counter

      line = 'a'
      line_no = 0
      do while (line /= '')
         read (handle, '(a80)') line
         line_no = line_no + 1
      end do

      line_no = line_no - 1
      backspace (handle)
      backspace (handle)

      read (handle, '(a80)') line
      n = len_trim(line)
      n = (n/4) + 1

      allocate (stacks(n))

      do while (line_no /= 1)
         line_no = line_no - 1
         backspace (handle)
         backspace (handle)
         read (handle, '(a80)') line
         ! print*, line
         do i = 1, n
            tmp = line(i*4 - 2:i*4 - 2)
            if (tmp /= ' ') then
               call stacks(i)%push(tmp)
            end if
         end do
      end do

      do while (line /= '')
         read (handle, '(a80)') line
      end do
   end subroutine read_stacks

   subroutine parse_move(handle, number, from, to, stat)
      !! Parse a single line from the input file
      integer, intent(inout) :: handle
         !! The unit to read from
      integer, intent(out) :: number
         !! The number of moves to do
      integer, intent(out) :: from
         !! The stack to move from
      integer, intent(out) :: to
         !! The stack to move to
      integer, intent(out) :: stat
         !! iostat from read

      character(len=4) :: dummy1, dummy2, dummy3
         !! Used for discarded parts of list directed read

      character(len=80) line
         !! A single line of input

      number = 0
      from = 0
      to = 0
      read (handle, '(a80)', iostat=stat) line
      if (stat /= 0) return
      read (line, *) dummy1, number, dummy2, from, dummy3, to

   end subroutine parse_move

end module day5_utils
