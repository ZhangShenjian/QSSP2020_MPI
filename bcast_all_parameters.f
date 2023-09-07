      subroutine bcast_all_parameters()
c
c     broadcast parameters get from 'qpgetinp.f' to all proc.
c
      use mpi
      use qpalloc
      implicit none
c
      integer*4 myrank,ierr
      integer*4 nparam_i
      parameter(nparam_i=17)
      integer*4 nparam_r
      parameter(nparam_r=20)
      integer*4 nparam_l
      parameter(nparam_l=5)
      integer*4 bcast_integer(nparam_i)
      real*8 bcast_real(nparam_r)
      logical*2 bcast_logical(nparam_l)
c
c     initialize
      call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)
c
      bcast_integer(:) = 0
      bcast_real(:) = 0.d0
      bcast_logical(:) = .false.
c
c     arrange data at master process
c
      if(myrank .eq. 0) then
          bcast_integer = (/ngrn,nt,ntcut,ntcutout,nf,nfcut,nbpf,
     &    lyadd,ipatha,ipathb,ldeggr,ldegmin,ldegcut,ldegmax,
     &    nr,ns,l0/)
          bcast_real = (/dt,dtout,df,fi,fcut,fgr,rratmos,depatmos,
     &    rearth,rr0,minpath,maxpath,
     &    slwmax,slwlwcut,slwupcut,f1corner,f2corner,
     &    dpr,qsmin,togsmin/)
          bcast_logical = (/selpsv,selsh,nogravity,freesurf,
     &    dispersion/)
      endif
c
c     broadcast temporal arrays
c
      call bcast_all_i(bcast_integer, nparam_i)
      call bcast_all_r(bcast_real, nparam_r)
      call bcast_all_l(bcast_logical, nparam_l)
c
      call bcast_all_i(icmp, 11)
c 
c     receive at slaver processes
c
      if(myrank .ne. 0) then
          ngrn = bcast_integer(1)
          nt = bcast_integer(2)
          ntcut = bcast_integer(3)
          ntcutout = bcast_integer(4)
          nf = bcast_integer(5)
          nfcut = bcast_integer(6)
          nbpf = bcast_integer(7)
          lyadd = bcast_integer(8)
          ipatha = bcast_integer(9)
          ipathb = bcast_integer(10)
          ldeggr = bcast_integer(11)
          ldegmin = bcast_integer(12)
          ldegcut = bcast_integer(13)
          ldegmax = bcast_integer(14)
          nr = bcast_integer(15)
          ns = bcast_integer(16)
          l0 = bcast_integer(17)
c          
          dt = bcast_real(1)
          dtout = bcast_real(2)
          df = bcast_real(3)
          fi = bcast_real(4)
          fcut = bcast_real(5)
          fgr = bcast_real(6)
          rratmos = bcast_real(7)
          depatmos = bcast_real(8)
          rearth = bcast_real(9)
          rr0 = bcast_real(10)
          minpath = bcast_real(11)
          maxpath = bcast_real(12)
          slwmax = bcast_real(13)
          slwlwcut = bcast_real(14)
          slwupcut = bcast_real(15)
          f1corner = bcast_real(16)
          f2corner = bcast_real(17)
          dpr = bcast_real(18)
          qsmin = bcast_real(19)
          togsmin = bcast_real(20)
c
          selpsv = bcast_logical(1)
          selsh = bcast_logical(2)
          nogravity = bcast_logical(3)
          freesurf = bcast_logical(4)
          dispersion = bcast_logical(5)
c
          call qplocalinit()
      endif
c
c     broadcast existing arrays
c 
      call bcast_all_i(grnsel, ngrn)
      call bcast_all_i(isg1, ngrn)
      call bcast_all_i(isg2, ngrn)
      call bcast_all_i(nsg, ngrn)
c
      call bcast_all_r(latr, nr)
      call bcast_all_r(lonr, nr)
      call bcast_all_r(tred, nr)
      call bcast_all_r(grndep, ngrn)
c
      call bcast_all_r(sfr, ns)
      call bcast_all_r(sft, ns)
      call bcast_all_r(sfp, ns)
      call bcast_all_r(mtt, ns)
      call bcast_all_r(mpp, ns)
      call bcast_all_r(mrr, ns)
      call bcast_all_r(mtp, ns)
      call bcast_all_r(mpr, ns)
      call bcast_all_r(mrt, ns)
      call bcast_all_r(lats, ns)
      call bcast_all_r(lons, ns)
      call bcast_all_r(deps, ns)
      call bcast_all_r(togs, ns)
      call bcast_all_r(trss, ns)
c
      call bcast_all_r(dp0, l0)
      call bcast_all_r(vp0, l0)
      call bcast_all_r(vs0, l0)
      call bcast_all_r(ro0, l0)
      call bcast_all_r(qp0, l0)
      call bcast_all_r(qs0, l0)
c
      call bcast_all_r(dp0up, l0)
      call bcast_all_r(vp0up, l0)
      call bcast_all_r(vs0up, l0)
      call bcast_all_r(ro0up, l0)
      call bcast_all_r(qp0up, l0)
      call bcast_all_r(qs0up, l0)
      call bcast_all_r(dp0lw, l0)
      call bcast_all_r(vp0lw, l0)
      call bcast_all_r(vs0lw, l0)
      call bcast_all_r(ro0lw, l0)
      call bcast_all_r(qp0lw, l0)
      call bcast_all_r(qs0lw, l0)
c
      end subroutine bcast_all_parameters