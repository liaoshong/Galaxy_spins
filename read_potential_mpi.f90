
program read_potential_files
  use mpi
  implicit none

  integer, parameter :: ngrid = 910
  integer, parameter :: file_num = 8
  character*200, parameter :: file_path = '/data/inspur_disk03/userdir/shliao/haoran_yunchong_IC/yunchong_tng_IC/IC/'
  character*200, parameter :: file_basename = 'ICs'

  character*200 filename, fnumber
  integer*4 local_x_start, local_nx, ny, nz
  integer*4 file_i, i, j, k
  real*8, allocatable :: phi(:,:,:)
  real*8, allocatable :: phi_tmp(:,:,:)

  integer*4 rank, num_procs, ierror

  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

  if (num_procs .ne. file_num) then
    print *, 'CPU number does not match file number!'
    call MPI_ABORT(MPI_COMM_WORLD, 2001, ierror)
  endif

  write(fnumber, '(I1)') rank
  filename = file_path(1:len_trim(file_path)) // '/' // file_basename(1:len_trim(file_basename)) // '_potential.' // fnumber(1:1)

  if (rank .eq. 0) then
    print *, 'CPU 0: Reading', filename
  endif

  open(1, file=filename, access='stream')
  read(1) local_x_start, local_nx, ny, nz
  allocate(phi_tmp(nz, ny, local_nx))
  allocate(phi(local_nx, ny, nz))       ! Need to consider the padded space when using FFTW
  read(1) phi_tmp

  do k=1, nz
  do j=1, ny
  do i=1, local_nx
    phi(i,j,k) = phi_tmp(k,j,i)
  enddo
  enddo
  enddo

  close(1)
  deallocate(phi_tmp)

  ! COMPUTATIONS...

  deallocate(phi)

  call MPI_FINALIZE(ierror)

end program
