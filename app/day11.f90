program day11
   use utils, only: get_input
   use day11_utils, only: Monkey, lcm
   use, intrinsic :: iso_fortran_env, only: int64
   implicit none

   character(len=80) :: filename
      !! The problem filename
   integer :: handle
      !! The unit to read from
   character(len=80) :: line
      !! A single line of input

   type(Monkey), allocatable, dimension(:) :: monkeys
      !! The monkeys with the items
   integer :: rounds
      !! Number of rounds
   integer, allocatable, dimension(:) :: divisors
      !! Divisors for calculating modulo to use

   integer :: n
      !! Number of monkeys
   integer :: m
      !! Counter for reading

   integer :: temp
      !! Temp int
   integer :: i, j
      !! Loop counter
   integer :: stat
      !! iostat result

   integer(int64) :: bigint
      !! Avoid overflow during large calculations at the end

   rounds = 10000

   call get_input(filename, handle)

   ! Count monkeys
   n = 0
   read (handle, '(a80)', iostat=stat) line
   do while (stat == 0)
      if (line(1:6) == 'Monkey') n = n + 1
      read (handle, '(a80)', iostat=stat) line
   end do
   rewind (handle)

   allocate (monkeys(n))
   allocate (divisors(n))
   do i = 1, n
      call monkeys(i)%init()
      ! Monkey id line
      read (handle, '(a80)') line
      ! Starting items
      read (handle, '(a80)') line
      m = (len_trim(line) - 16)/4
      do j = 1, m
         read (line(15 + 4*j:16 + 4*j), *) temp
         call monkeys(i)%items%push(temp)
      end do
      ! Operation
      read (handle, '(a80)') line
      monkeys(i)%op = line(24:24)
      if (line(26:28) == 'old') then
         monkeys(i)%scalar = -1
      else
         read (line(26:80), *) temp
         monkeys(i)%scalar = temp
      end if
      ! Test divisible
      read (handle, '(a80)') line
      read (line(22:80), *) temp
      monkeys(i)%divisor = temp
      divisors(i) = temp
      ! Test true
      read (handle, '(a80)') line
      read (line(30:80), *) temp
      monkeys(i)%pass_true = temp + 1
      ! Test false
      read (handle, '(a80)') line
      read (line(31:80), *) temp
      monkeys(i)%pass_false = temp + 1
      ! Blank line
      if (i /= n) read (handle, '(a80)') line
   end do

   temp = lcm(divisors)
   do i = 1, n
      monkeys(i)%worry_reductor = temp
   end do

   ! Run rounds
   do i = 1, rounds
      do j = 1, n
         call monkeys(j)%pass(temp, m)
         do while (temp >= 0 .and. m >= 0)
            call monkeys(m)%items%push(temp)
            call monkeys(j)%pass(temp, m)
         end do
      end do
   end do

   ! Find monkeys with most inspections (m > temp are top two)
   m = -1
   temp = -1
   j = -1
   do i = 1, n
      print *, "Monkey ", i, " inspected ", monkeys(i)%inspections
      if (monkeys(i)%inspections > temp) temp = monkeys(i)%inspections
      if (temp > m) then
         j = temp
         temp = m
         m = j
      end if
   end do

   bigint = temp
   bigint = bigint*m
   print *, "Monkey business score..: ", bigint, "(", temp, m, ")"
end program day11
