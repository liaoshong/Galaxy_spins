
program read_potential_files
  implicit none

  ! The following parameters are specified to run the potential field of TNG100-3 simulation.
  integer, parameter :: ngrid = 910
  integer, parameter :: file_num = 8
  character*200, parameter :: file_path = '/data/inspur_disk03/userdir/shliao/haoran_yunchong_IC/yunchong_tng_IC/IC/'
  character*200, parameter :: file_basename = 'ICs'

  character*200 filename, fnumber
  integer*4 local_x_start, local_nx, ny, nz
  integer*4 file_i, i, j, k
  real*8, allocatable :: phi(:,:,:)     ! To save the global potential field
  real*8, allocatable :: phi_tmp(:,:,:) ! To save the potential field stored in each file

  allocate(phi(ngrid, ngrid, ngrid))

  do file_i=1,file_num
    write(fnumber, '(I1)') file_i-1
    filename = file_path(1:len_trim(file_path)) // '/' // file_basename(1:len_trim(file_basename)) // '_potential.' // fnumber(1:1)
    print *, 'Reading', filename
    
    open(1, file=filename, access='stream')
    read(1) local_x_start, local_nx, ny, nz
    allocate(phi_tmp(nz, ny, local_nx))
    read(1) phi_tmp

    ! The potential field in files are stored in row-major order, but fortran
    ! reads in column-major order. Transform in the following loops.
    do k=1, nz
    do j=1, ny
    do i=1, local_nx
      phi(local_x_start+i,j,k) = phi_tmp(k,j,i)
    enddo
    enddo
    enddo

    close(1)
    deallocate(phi_tmp)
  end do

  deallocate(phi)

end program
