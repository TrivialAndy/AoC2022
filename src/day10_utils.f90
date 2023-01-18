module day10_utils
   implicit none
   private
   public :: Comp

   type :: Comp
      !! Implementation of the computer
      integer :: clock
         !! Current clock cylce
      integer :: register
         !! Current register value
      integer :: tot_strength
         !! Total signal strength so far
   contains
      procedure :: init
      procedure :: inc_clock
      procedure :: noop
      procedure :: addx
      procedure :: signal_strength
      procedure :: draw
   end type Comp

contains
   subroutine init(self)
   !! Initialise the Comp
      class(Comp), intent(inout) :: self
      !! Comp to initialise

      self%clock = 0
      self%register = 1
      self%tot_strength = 0
   end subroutine init

   subroutine inc_clock(self)
   !! Increment the clock by 1
      class(Comp), intent(inout) :: self
      !! The Comp to inc the clock of

      call self%draw()

      self%clock = self%clock + 1
      if (modulo(self%clock - 20, 40) == 0) then
         self%tot_strength = self%tot_strength + self%signal_strength()
      end if

   end subroutine inc_clock

   subroutine noop(self)
      !! Perform the noop
      class(Comp), intent(inout) :: self
         !! The Comp to noop

      call self%inc_clock()
   end subroutine noop

   subroutine addx(self, val)
      !! Add a value to the register
      class(Comp), intent(inout) :: self
         !! Comp to add to
      integer, intent(in) :: val
         !! Value to add to the register

      call self%inc_clock()
      call self%inc_clock()
      self%register = self%register + val
   end subroutine addx

   function signal_strength(self) result(retval)
      !! Calculate the signal strength
      class(Comp), intent(in) :: self
         !! The Comp to get strength of
      integer :: retval
         !! The signal strength

      retval = self%clock*self%register
   end function signal_strength

   subroutine draw(self)
      !! Draw a char to the screen
      class(Comp), intent(in) :: self
         !! The Comp to draw

      integer :: x
         !! x position of the display to write
      character :: c
         !! The char to write

      x = modulo(self%clock, 40)

      if (x == 0) print *, ' '
      if (abs(x - self%register) <= 1) then
         c = '#'
      else
         c = '.'
      end if
      write (*, '(a1)', advance='no') c

   end subroutine draw
end module day10_utils
