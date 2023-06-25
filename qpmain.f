      program qssp
      use mpi
      use qpalloc
      implicit none
c
c     work space
c
      integer*4 ig,ierr,runtime
      integer*4 time
      character*80 inputfile
c     MPI variables
      integer*4 myrank, numprocs, ierr_mpi
      integer*4 tag, status(MPI_STATUS_SIZE)
c
c     Initialize MPI
c
      call MPI_Init(ierr_mpi)
c     Get the rank and number of processes
      call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr_mpi)
      call MPI_Comm_size(MPI_COMM_WORLD, numprocs, ierr_mpi)
c      write(*,*)'rank',myrank, 'of', numprocs, 'processors'
c
      if (myrank == 0) then
c     read input file file
      write(*,*)'######################################################'
      write(*,*)'#                                                    #'
      write(*,*)'#               Welcome to the program               #'
      write(*,*)'#                                                    #'
      write(*,*)'#    QQQQ         SSSSS        SSSSS       PPPPP     #'
      write(*,*)'#   Q    Q       S            S            P    P    #'
      write(*,*)'#   Q    Q        SSSS         SSSS        PPPPP     #'
      write(*,*)'#   Q   QQ            S            S       P         #'
      write(*,*)'#    QQQQQ       SSSSS        SSSSS        P         #'
      write(*,*)'#                                                    #'
      write(*,*)'#          Complete synthetic seismograms            #'
      write(*,*)'#      (displacement/strain/stress/rotation)         #' 
      write(*,*)'#                     based on                       #'
      write(*,*)'#          a spherically symmetric earth model       #'
      write(*,*)'#                                                    #'
      write(*,*)'#                  (Version 2020)                    #'
      write(*,*)'#   Last update (correction of errors): 2020-04-14   #'
      write(*,*)'#                                                    #'
      write(*,*)'#                      by                            #'
      write(*,*)'#                 Rongjiang Wang                     #'
      write(*,*)'#              (wang@gfz-potsdam.de)                 #'
      write(*,*)'#                                                    #'
      write(*,*)'#              Helmholtz Centre Potsdam              #'
      write(*,*)'#    GFZ German Research Centre for Geosciences      #'
      write(*,*)'#           Last modified: September 2017            #'
      write(*,*)'#                                                    #'
      write(*,*)'######################################################'
      write(*,*)'                                                      '
      write(*,'(a,$)')' the input data file is '
      read(*,'(a)')inputfile
      runtime=time()
c
      open(10,file=inputfile,status='old')
      call qpgetinp(10)
      close(10)
      endif
c      
      call MPI_BARRIER(MPI_COMM_WORLD, ierr_mpi)
c
c     IMPORTANT: broadcast parameters getting from input file
c  
      call bcast_all_parameters()
c
c     initalize arrays not used in 'qpgetinp'
c
      call qpinitarr(ierr)
c
c     computation at each process
c
      call qpsublayer(ierr)
c
      igfirst=0
      do ig=ngrn,1,-1
        if(grnsel(ig).eq.1)igfirst=ig
      enddo
      iglast=ngrn+1
      do ig=1,ngrn
        if(grnsel(ig).eq.1)iglast=ig
      enddo
c
      call synchronize_all()
      do ig=1,ngrn
        if(grnsel(ig).eq.1)then
          lys=lygrn(ig)
          call qpgrnspec(ig)
        endif
      enddo
c      
c      call qpwvint(ierr)
c      call qpfftinv(ierr)
c
      if(myrank==0)then
      runtime=time()-runtime
      write(*,'(a)')' #############################################'
      write(*,'(a)')' #                                           #'
      write(*,'(a)')' #      End of computations with qssp2020    #'
      write(*,'(a)')' #                                           #'
      write(*,'(a,i10,a)')' #       Run time: ',runtime,
     +                                           ' sec            #'
      write(*,'(a)')' #############################################'
      endif
c
c     Finalize MPI
c
      call MPI_Finalize(ierr_mpi) 
c
      stop
      end
