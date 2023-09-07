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
      call MPI_INIT(ierr_mpi)
c     Get the rank and number of processes
      call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr_mpi)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr_mpi)
c
c     I/O at master process
      if (myrank .eq. 0) then
c     read input file file
c      write(*,*)'#####################################################'
c      write(*,*)'#                                                   #'
c      write(*,*)'#               Welcome to the program              #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#    QQQQ         SSSSS        SSSSS       PPPPP    #'
c      write(*,*)'#   Q    Q       S            S            P    P   #'
c      write(*,*)'#   Q    Q        SSSS         SSSS        PPPPP    #'
c      write(*,*)'#   Q   QQ            S            S       P        #'
c      write(*,*)'#    QQQQQ       SSSSS        SSSSS        P        #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#          Complete synthetic seismograms           #'
c      write(*,*)'#      (displacement/strain/stress/rotation)        #' 
c      write(*,*)'#                     based on                      #'
c      write(*,*)'#          a spherically symmetric earth model      #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#             (Parallel Version 2023)               #'
c      write(*,*)'#               Update: 2023-09-07                  #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#                by Shenjian Zhang                  #'
c      write(*,*)'#            (zhangsj@sustech.edu.cn)               #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#              (Basic Version 2020)                 #'
c      write(*,*)'#   Last update (correction of errors): 2020-04-14  #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#                      by                           #'
c      write(*,*)'#                 Rongjiang Wang                    #'
c      write(*,*)'#              (wang@gfz-potsdam.de)                #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#              Helmholtz Centre Potsdam             #'
c      write(*,*)'#    GFZ German Research Centre for Geosciences     #'
c      write(*,*)'#           Last modified: September 2017           #'
c      write(*,*)'#                                                   #'
c      write(*,*)'#####################################################'
      write(*,*)'                                                      '
      write(*,'(a,$)')' the input data file is '
      read(*,'(a)')inputfile
      runtime=time()
c
c     read parameter file
c
      open(10,file=inputfile,status='old')
      call qpgetinp(10)
      close(10)
      endif
c      
      call synchronize_all()
c
c     IMPORTANT: broadcast parameters to all processes
c  
      call bcast_all_parameters()
c
c     initalize arrays not used in 'qpgetinp'
c
      call qpinitarr(ierr)
c
c     begin computation at each process
c
      call qpsublayer(ierr)
c
c     compute Green's function sets
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
c     compute spectral solutions from spherical harmonic factors in
c     Green's function sets
c
      call qpwvint(ierr)
c
c     Obtain time series by backward FFT
c
      if(myrank .eq. 0)then
        call qpfftinv(ierr)
      endif
c
      if(myrank.eq.0)then
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
      call MPI_FINALIZE(ierr_mpi) 
c
      stop
      end
