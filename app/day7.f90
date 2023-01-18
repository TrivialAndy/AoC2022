program day7
   use utils, only: get_input
   use day7_utils, only: Dir
   implicit none

   character(len=80) :: filename
      !! The filename
   integer :: handle
      !! Unit to read input from

   character(len=80) :: line
      !! Single line of input
   integer :: stat
      !! iostat value

   type(Dir), target :: root
      !! The root directory
   type(Dir), pointer :: wd
      !! Working directory

   character(len=50) :: name
      !! Name to generate a file or dir with
   integer :: filesize
      !! Temporary int for file sizes

   integer :: total
      !! Sum of filesizes for problem 1
   integer :: idx
      !! Index of children in return from calls to navigate

   integer :: available
      !! Total available disk size
   integer :: update_size
      !! Size of update to install
   integer :: to_delete
      !! Size of smallest directory to delete

   available = 70000000
   update_size = 30000000

   call get_input(filename, handle)

   ! Parse the file system
   read (handle, '(a80)', iostat=stat) line
   root%name = '/'
   nullify (root%parent)
   wd => root
   do while (stat == 0)
      ! Check for command
      if (line(1:1) == '$') then
         if (line(3:4) == 'cd') then
            call wd%navigate(line(6:80), idx)
            select case (idx)
            case (-1)
               wd => root
            case (0)
               wd => wd%parent
            case default
               wd => wd%dirs(idx)
            end select
         end if
         ! Assume ls always preceeds other input lines... it can be ignored
      else if (line(1:4) == 'dir ') then
         call wd%add_dir(line(5:80))
      else
         read (line, *) filesize, name
         call wd%add_file(name, filesize)
      end if
      ! call root%disp(1)
      read (handle, '(a80)', iostat=stat) line
   end do

   call root%get_size(filesize)
   available = available - filesize
   to_delete = filesize
   total = 0

   wd => root
   do while (.true.)
      call wd%get_size(filesize)
      if (available + filesize > update_size .and. filesize < to_delete) &
         to_delete = filesize
      if (filesize <= 100000) total = total + filesize
      if (wd%num_dirs > 0) then
         ! print*, trim(wd%name),' ', wd%num_dirs, ' ', size(wd%dirs) 
         wd => wd%dirs(1)
      else if (associated(wd, target=root)) then
         exit
      else
         do while (.true.)
            if (.not. associated(wd%parent)) then
               nullify (wd)
               exit
            end if
            call wd%parent%navigate(wd%name, idx)
            if (idx == wd%parent%num_dirs) then
               wd => wd%parent
            else
               wd => wd%parent%dirs(idx + 1)
               exit
            end if
         end do
         if (.not. associated(wd)) exit
      end if
   end do

   print *, "Total filesize for part 1: ", total
   print *, "deleting ", to_delete

end program day7
