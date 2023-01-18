module day11_utils
   use, intrinsic :: iso_fortran_env, only: int64, int32
   implicit none
   private
   public :: Monkey, lcm

   type, public :: Queue
      !! A dynamic queue
      integer, allocatable, dimension(:) :: values
         !! The items in the queue
      integer :: head
         !! The first item in the queue
      integer :: tail
         !! The last item in the queue
      integer :: n
         !! The number of items in the queue
   contains
      procedure :: init => init_queue
      procedure :: push
      procedure :: pop
      procedure :: grow
   end type Queue

   type :: Monkey
      !! A monkey with some items
      type(Queue) :: items
         !! The items the monkey has
      character :: op
         !! The operation to perform
      integer :: scalar
         !! The scalar for the operation
      integer :: divisor
         !! The check for where to pass
      integer :: pass_true
         !! Where to pass if divisible by divisor
      integer :: pass_false
         !! Where to pass if not divisible by divisor
      integer :: inspections
         !! Number of times monkey inspects an item
      integer :: worry_reductor
         !! Number to divide worry by
   contains
      procedure :: init => init_monkey
      procedure :: pass
   end type Monkey
contains
   subroutine init_queue(self)
      !! Initialise a queue
      class(Queue), intent(inout) :: self
         !! The queue to init

      self%head = 1
      self%tail = 0
      self%n = 0
      allocate (self%values(10))
   end subroutine init_queue

   subroutine push(self, val)
      !! Add a value to the queue
      class(Queue), intent(inout) :: self
         !! The queue to append to
      integer, intent(out) :: val
         !! The value to append

      if (self%n == size(self%values)) call self%grow()

      if (self%tail == size(self%values)) self%tail = 0
      self%tail = self%tail + 1
      self%n = self%n + 1
      self%values(self%tail) = val
   end subroutine push

   subroutine pop(self, val)
      !! Pop a value from the queue
      class(Queue), intent(inout) :: self
         !! The queue to pop from
      integer, intent(out) :: val
         !! The popped value

      if (self%n == 0) then
         val = -1
         return
      end if

      val = self%values(self%head)
      self%n = self%n - 1
      if (self%head == size(self%values)) self%head = 0
      self%head = self%head + 1
   end subroutine pop

   subroutine grow(self)
      !! Double the size of the queue
      class(Queue), intent(inout) :: self
         !! The queue to enlarge

      integer, allocatable, dimension(:) :: temp
         !! Temp storage for the reallocate

      integer :: i
         !! Loop counter

      allocate (temp(self%n))
      do i = 1, self%n
         temp(i) = self%values(1 + modulo(self%head + i - 2, size(self%values)))
      end do

      deallocate (self%values)
      allocate (self%values(self%n*2))

      self%head = 1
      self%tail = self%n
      self%values(1:self%n) = temp
   end subroutine grow

   subroutine init_monkey(self)
      !! Initialise a monkey with default values
      class(Monkey), intent(inout) :: self
         !! The monkey

      call self%items%init()
      self%divisor = 1
      self%pass_false = 1
      self%pass_true = 1
      self%inspections = 0
      self%op = '+'
      self%scalar = 1
      self%worry_reductor = 1
   end subroutine init_monkey

   subroutine pass(self, val, to)
      !! Inspect a value, update and find who to pass to (pops value)
      class(Monkey), intent(inout) :: self
         !! The monkey to pass from
      integer, intent(out) :: val
         !! The value to pass
      integer, intent(out) :: to
         !! The monkey to pass to

      integer(int64) :: scalar
         !! The scalar to use

      integer(int64) :: overflow_guard

      if (self%items%n == 0) then
         to = -1
         val = 0
         return
      end if

      call self%items%pop(val)

      if (self%scalar == -1) then
         scalar = val
      else
         scalar = self%scalar
      end if

      overflow_guard = int(val, int64)
      if (self%op == '*') then
         overflow_guard = overflow_guard*scalar
      else if (self%op == '+') then
         overflow_guard = overflow_guard + scalar
      else
         to = 0
         val = -1
         return
      end if

      if (self%worry_reductor < 0) then
         overflow_guard = overflow_guard/3
      else
         overflow_guard = modulo(overflow_guard, self%worry_reductor)
      end if
      val = int(overflow_guard, int32)

      if (modulo(val, self%divisor) == 0) then
         to = self%pass_true
      else
         to = self%pass_false
      end if

      self%inspections = self%inspections + 1
   end subroutine pass

   function lcm(val) result(retval)
      !! Find the lowest common multiple of two numbers
      integer, intent(in), dimension(:) :: val
      integer :: retval

      integer, allocatable, dimension(:) :: working
         !! Working array

      integer :: m
         !! Max val so far
      integer :: l
         !! Least val so far

      integer :: i
         !! Loop counter

      allocate (working(size(val)))
      working = 0

      l = 0
      m = 1
      do while (l /= m)
         l = working(1)
         do i = 1, size(working)
            if (working(i) < m) working(i) = working(i) + val(i)
            if (working(i) > m) m = working(i)
            if (working(i) < l) l = working(i)
         end do
      end do
      retval = l
   end function lcm
end module day11_utils
