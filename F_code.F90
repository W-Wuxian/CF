program F_code

  use, intrinsic ::  iso_c_binding
  use mpi
  !include 'mpif'
  implicit none

  type, bind(c) :: mat
     type(c_ptr) :: iv
     type(c_ptr) :: jv
     type(c_ptr) :: vv
     integer(c_int) :: m
     integer(c_int) :: nnz
  end type mat

  type, bind(c) :: parameters
     real(c_double) :: alpha
     integer(c_int) :: comm
     integer(c_int) :: fcomm
  end type parameters

  interface
     subroutine initmat_c(Amat) &
          bind(c, name='initmat')
       use iso_c_binding
       import mat
       implicit none
       type(c_ptr), value :: Amat
     end subroutine initmat_c
  end interface

  interface
     subroutine driver_mat_c(inA, outX, inParams) &
          bind(c, name='driver_mat')
       use iso_c_binding
       import mat
       import parameters
       implicit none
       type(c_ptr), value :: inA
       type(c_ptr), value :: outX
       type(c_ptr), value :: inParams
     end subroutine driver_mat_c
  end interface

  interface
     subroutine driver_mat_mpi_c(inA, outX, inParams) &
          bind(c, name='driver_mat_mpi')
       use iso_c_binding
       import mat
       import parameters
       implicit none
       type(c_ptr), value :: inA
       type(c_ptr), value :: outX
       type(c_ptr), value :: inParams
     end subroutine driver_mat_mpi_c
  end interface

  integer, parameter :: m=2
  integer, parameter :: nnz=2
  
  type(mat), target :: A, X
  type(parameters), target :: params

  real(c_double), dimension(:), pointer :: av
  integer(c_int), dimension(:), pointer :: ai,aj

  real(c_double), dimension(:), pointer :: xv
  integer(c_int), dimension(:), pointer :: xi,xj

  integer :: ierr
  integer :: myid
  integer :: comm
  integer :: fcomm
  integer :: Np
    ! Init MPI
  Call MPI_INIT(ierr)
  fcomm = MPI_COMM_WORLD
  comm = fcomm
  print*,'COMM', fcomm
  if (ierr /= MPI_SUCCESS) ierr = -1
  if (ierr < 0) goto 9999
  
  Call MPI_COMM_RANK(comm, myid, ierr)
  if (ierr /= MPI_SUCCESS) ierr = -1
  if (ierr < 0) goto 9999

  Call MPI_COMM_SIZE(comm, Np, ierr)
  if (Np /= 2) then
     stop "Error: this exemple should be launched with 2 processes"
  end if
  if (ierr /= MPI_SUCCESS) ierr = -1
  if (ierr < 0) goto 9999
  Print*,'Np', Np
  
  call initmat(A)
  call initmat(X)

  A%m=m;A%nnz=nnz
  X%m=m;X%nnz=nnz
  
  allocate(av(nnz),ai(nnz),aj(nnz))
  allocate(xv(nnz),xi(nnz),xj(nnz))

  A%iv = c_loc(ai)
  A%jv = c_loc(aj)
  A%vv = c_loc(av)
  X%iv = c_loc(xi)
  X%jv = c_loc(xj)
  X%vv = c_loc(xv)
  
  ai(1)=0;aj(1)=0;av(1)=1.0d0
  ai(2)=1;aj(2)=1;av(2)=1.0d0

  print*,'ai(:)',ai(:)
  print*,'aj(:)',aj(:)
  print*,'av(:)',av(:)
  
  params%alpha = 2.0d0
  params%comm  = comm
  params%fcomm = fcomm

  print*,'rank',myid
  
  call driver_mat_mpi(A, X, params)
  print*,myid,'xi(:)',xi(:)
  print*,myid,'xj(:)',xj(:)
  print*,myid,'xv(:)',xv(:)
  print*,'fff'

  ! FINALIZE MPI
  Call MPI_FINALIZE(ierr)
  if (ierr /= 0) then
     print*, "Problem with mpi finalize"
  end if

9999 Continue
  
  deallocate(av,ai,aj,xv,xi,xj)

  if (ierr /= 0) then
     print*, "error"
  end if

  
contains

  subroutine initmat(Amat)
    use iso_c_binding
    implicit none
    type(mat), intent(inout), target :: Amat

    call initmat_c(c_loc(Amat))
  end subroutine initmat

  subroutine driver_mat(inA, outX, inParams)
    use iso_c_binding
    implicit none
    type(mat), intent(inout), target :: inA
    type(mat), intent(inout), target :: outX
    type(parameters), intent(inout), target :: inParams
    print*,'ininin'
    call driver_mat_c(c_loc(inA), c_loc(outX), c_loc(inParams))
  end subroutine driver_mat

  subroutine driver_mat_mpi(inA, outX, inParams)
    use iso_c_binding
    implicit none
    type(mat), intent(inout), target :: inA
    type(mat), intent(inout), target :: outX
    type(parameters), intent(inout), target :: inParams
    print*,'ininin'
    call driver_mat_mpi_c(c_loc(inA), c_loc(outX), c_loc(inParams))
  end subroutine driver_mat_mpi

end program F_code
