module day7_utils
   implicit none
   private
   public :: File, Dir

   type :: File
      character(30) :: name
      integer :: mem
   end type File

   type :: Dir
      type(Dir), pointer :: parent
      type(File), allocatable, dimension(:) :: files
      type(Dir), allocatable, dimension(:) :: dirs
      character(len=30) :: name
      integer :: num_files
      integer :: num_dirs
   contains
      procedure :: get_size
      procedure :: add_file
      procedure :: add_dir
      procedure :: navigate
      procedure :: disp
   end type Dir
contains
   recursive subroutine get_size(self, dir_size)
      !! Get the size of the directory
      class(Dir), intent(inout) :: self
         !! Dir to check size of
      integer, intent(out) :: dir_size
         !! Size of the directory

      integer :: tmp_size
         !! Temp storage for sizes
      integer :: i
         !! Loop counter

      dir_size = 0
      if (allocated(self%dirs)) then
         do i = 1, self%num_dirs
            call self%dirs(i)%get_size(tmp_size)
            dir_size = dir_size + tmp_size
         end do
      end if

      if (allocated(self%files)) then
         do i = 1, self%num_files
            dir_size = dir_size + self%files(i)%mem
         end do
      end if
   end subroutine get_size

   subroutine add_file(self, name, mem)
      !! Add a file to the directory
      class(Dir), intent(inout) :: self
         !! The directory
      character(len=*), intent(in) :: name
         !! The filename
      integer, intent(in) :: mem
         !! The filesize

      type(File), allocatable, dimension(:) :: tmp_f
         !! Temp files for reallocating self%files

      integer :: i
         !! Loop counter
      ! print *, "Adding file: ", trim(name)

      if (.not. allocated(self%files)) then
         allocate (self%files(10))
         self%num_files = 0
      end if
      self%num_files = self%num_files + 1
      if (self%num_files > size(self%files)) then
         allocate (tmp_f(size(self%files)))
         do i = 1, size(self%files)
            tmp_f(i) = self%files(i)
         end do

         deallocate (self%files)
         allocate (self%files(self%num_files*2))
         do i = 1, size(tmp_f)
            self%files(i) = tmp_f(i)
         end do
         deallocate (tmp_f)
      end if

      self%files(self%num_files)%name = name
      self%files(self%num_files)%mem = mem

   end subroutine add_file

   subroutine add_dir(self, name)
      !! Add a file to the directory
      class(Dir), intent(inout), target :: self
         !! The directory
      character(len=*), intent(in) :: name
         !! The dir name

      type(Dir), allocatable, dimension(:) :: tmp_d
         !! Temp dir for reallocating self%dirs

      integer :: i
         !! Loop counter

      if (.not. allocated(self%dirs)) then
         allocate (self%dirs(10))
         self%num_dirs = 0
      end if

      self%num_dirs = self%num_dirs + 1
      if (self%num_dirs > size(self%dirs)) then
         allocate (tmp_d(size(self%dirs)))
         do i = 1, size(self%dirs)
            tmp_d(i) = self%dirs(i)
         end do

         deallocate (self%dirs)
         allocate (self%dirs(self%num_dirs*2))
         do i = 1, size(tmp_d)
            self%dirs(i) = tmp_d(i)
         end do
         deallocate (tmp_d)
      end if

      self%dirs(self%num_dirs)%name = name
      self%dirs(self%num_dirs)%num_dirs = 0
      self%dirs(self%num_dirs)%num_files = 0
      self%dirs(self%num_dirs)%parent => self
      self%dirs(self%num_dirs)%parent = self

      ! print *, trim(name), " added to ", trim(self%name)
   end subroutine add_dir

   subroutine navigate(self, name, idx)
      !! Find the child matching the name
      class(Dir), intent(in) :: self
         !! Curent directory
      character(len=*), intent(in) :: name
         !! Name to search for

      integer, intent(out) :: idx
         !! Index of resulting child

      integer :: i
         !! Loop counter

      ! print *, "Nav: ", trim(self%name), " with ", self%num_dirs, &
      !    " looking for ", trim(name)
      if (trim(name) == '..') then
         idx = 0
         return
      else if (trim(name) == '/') then
         idx = -1
         return
      end if

      do i = 1, self%num_dirs
         if (trim(self%dirs(i)%name) == trim(name)) then
            idx = i
            return
         end if
      end do
      print *, "not found... ", name
      do i = 1, self%num_dirs
         print *, self%dirs(i)%name
      end do
   end subroutine navigate

   recursive subroutine disp(self, prefix)
      !! Print the directory structure
      class(Dir), intent(in) :: self
         !! THe directory to print
      integer, intent(in) :: prefix
         !! The number of spaces to pad with (increases with recursion)

      character(len=prefix+1) :: pad
         !! The whitespace to pad with

      integer :: i
         !! Loop counter

      pad = ' '
      print *, pad(2:len(pad)), trim(self%name), '/'
      if (self%num_files > 0) then
         do i = 1, self%num_files
            print *, pad, trim(self%files(i)%name)
         end do
      end if
      if (self%num_dirs > 0) then
         do i = 1, self%num_dirs
            call self%dirs(i)%disp(prefix + 1)
         end do
      end if

   end subroutine disp

end module day7_utils
