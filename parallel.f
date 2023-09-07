      subroutine bcast_all_i(buffer, countval)
      use mpi
      implicit none
      integer*4 countval,myrank
      integer*4 buffer(countval)
      integer*4 ierr_mpi
c
      call MPI_BCAST(buffer, countval, MPI_INTEGER, 0,
     &     MPI_COMM_WORLD, ierr_mpi)
c
      end subroutine bcast_all_i
c
c
c
      subroutine bcast_all_r(buffer, countval)
      use mpi
      implicit none
      integer*4 countval
      real*8 buffer(countval)
      integer*4 ierr_mpi
c
      call MPI_BCAST(buffer, countval, MPI_DOUBLE_PRECISION, 0,
     &     MPI_COMM_WORLD, ierr_mpi)
c
      end subroutine bcast_all_r
c
c
c
      subroutine bcast_all_l(buffer, countval)
      use mpi
      implicit none
      integer*4 countval
      logical*2 buffer(countval)
      integer*4 ierr_mpi
c
      call MPI_BCAST(buffer, countval, MPI_LOGICAL, 0,
     &     MPI_COMM_WORLD, ierr_mpi)
c
      end subroutine bcast_all_l
c
c
c
      subroutine bcast_all_c(buffer, countval)
      use mpi
      implicit none
      integer*4 countval
      complex*16 buffer(countval)
      integer*4 ierr_mpi
c
      call MPI_BCAST(buffer, countval, MPI_DOUBLE_COMPLEX, 0,
     &     MPI_COMM_WORLD, ierr_mpi)
c

      end subroutine bcast_all_c
c
c
c
      subroutine synchronize_all()
      use mpi
c
      integer*4 ierr_mpi
      call MPI_BARRIER(MPI_COMM_WORLD, ierr_mpi)
      if(ierr_mpi .ne. 0) then
          stop 'Error synchronize'
      endif
c
      end subroutine synchronize_all