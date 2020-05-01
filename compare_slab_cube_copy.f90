
program read_potential_files
  implicit none

  ! The following parameters are specified to run the potential field of TNG100-3 simulation.
  integer, parameter :: ngrid = 910
  integer, parameter :: file_num = 8
  character*200, parameter :: slab_file_path = '/data/inspur_disk03/userdir/shliao/haoran_yunchong_IC/yunchong_tng_IC/IC/'
  character*200, parameter :: slab_file_basename = 'ICs'

  character*200 filename, fnumber, redshift
  integer*4 local_x_start, local_nx, ny, nz
  integer*4 file_i, i, j, k
  real*8, allocatable :: phi_slab(:,:,:)     ! To save the global potential field
  real*8, allocatable :: phi_tmp_slab(:,:,:) ! To save the potential field stored in each file

  character*200, parameter :: cube_file_path = '/data/inspur_disk03/userdir/shliao/haoran_yunchong_IC/yunchong_tng_IC/slab_to_cube/res'
  real*4, parameter :: ic_redshift = 127.0
  integer*4, parameter :: image_per_dim = 2

  real*4, allocatable :: phi_cube(:,:,:)
  real*4, allocatable :: phi_tmp_cube(:,:,:)
  integer*4 image_x, image_y, image_z, image_count
  integer*4 file_size

  ! READ SLAB FILES
  allocate(phi_slab(ngrid, ngrid, ngrid))

  do file_i=1,file_num
    write(fnumber, '(I1)') file_i-1
    filename = slab_file_path(1:len_trim(slab_file_path)) // '/' // slab_file_basename(1:len_trim(slab_file_basename)) // '_potential.' // fnumber(1:1)
    print *, 'Reading', filename
    
    open(1, file=filename, access='stream')
    read(1) local_x_start, local_nx, ny, nz
    allocate(phi_tmp_slab(nz, ny, local_nx))
    read(1) phi_tmp_slab

    ! The potential field in files are stored in row-major order, but fortran
    ! reads in column-major order. Transform in the following loops.
    do k=1, nz
    do j=1, ny
    do i=1, local_nx
      phi_slab(local_x_start+i,j,k) = phi_tmp_slab(k,j,i)
    enddo
    enddo
    enddo

    close(1)
    deallocate(phi_tmp_slab)
  end do

  ! READ CUBE FILES
  allocate(phi_cube(ngrid, ngrid, ngrid))  

  do image_z=1,image_per_dim
    do image_y=1,image_per_dim
      do image_x=1,image_per_dim
        image_count = (image_x-1) + (image_y-1)*image_per_dim + (image_z-1)*image_per_dim**2 + 1
        write(redshift, '(E1.3)') ic_redshift
        write(fnumber, '(I1)') image_count
        filename = cube_file_path(1:len_trim(cube_file_path)) // '/' // redshift(1:len_trim(redshift)) // '_phi1_' // fnumber(1:1) // '.bin'
        print *, 'Reading', filename

        inquire(file=filename, size=file_size)
        print *, file_size

!        open(1, file=filename, access='stream')
!        read(1) phi_tmp_cube

      end do
    end do
  end do


  ! COMPARE SLAB FIELD AND CUBE FIELD


  ! FREE MEMORY
  deallocate(phi_slab)
  deallocate(phi_cube)

end program
