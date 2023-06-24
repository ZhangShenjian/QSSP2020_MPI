      subroutine qplocalinit()
      use qpalloc
      implicit none
      integer*4 ierr

      allocate(grndep(ngrn),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: grndep not allocated!'
      allocate(grnsel(ngrn),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: grnsel not allocated!'
c      
      allocate(isg1(ngrn),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: isg1 not allocated!'
      allocate(isg2(ngrn),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: isg2 not allocated!'
      allocate(nsg(ngrn),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: nsg not allocated!'
c
      allocate(sfr(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: sfr not allocated!'
      allocate(sft(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: sft not allocated!'
      allocate(sfp(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: sfp not allocated!'
c
      allocate(mrr(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: mrr not allocated!'
      allocate(mtt(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: mtt not allocated!'
      allocate(mpp(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: mpp not allocated!'
      allocate(mrt(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: mrt not allocated!'
      allocate(mpr(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: mpr not allocated!'
      allocate(mtp(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: mtp not allocated!'
      allocate(lats(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lats not allocated!'
      allocate(lons(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lons not allocated!'
      allocate(deps(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: deps not allocated!'
      allocate(togs(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: togs not allocated!'
      allocate(trss(ns),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: trss not allocated!'
c
      allocate(latr(nr),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: latr not allocated!'
      allocate(lonr(nr),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lonr not allocated!'
      allocate(tred(nr),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: tred not allocated!'
      allocate(rname(nr),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: rname not allocated!'
c
      allocate(dp0(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: dp0 not allocated!'
      allocate(vp0(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: vp0 not allocated!'
      allocate(vs0(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: vs0 not allocated!'
      allocate(ro0(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: ro0 not allocated!'
      allocate(qp0(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: qp0 not allocated!'
      allocate(qs0(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: qs0 not allocated!'
c
      allocate(dp0up(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: dp0up not allocated!'
      allocate(vp0up(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: vp0up not allocated!'
      allocate(vs0up(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: vs0up not allocated!'
      allocate(ro0up(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: ro0up not allocated!'
      allocate(qp0up(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: qp0up not allocated!'
      allocate(qs0up(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: qs0up not allocated!'
c
      allocate(dp0lw(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: dp0lw not allocated!'
      allocate(vp0lw(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: vp0lw not allocated!'
      allocate(vs0lw(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: vs0lw not allocated!'
      allocate(ro0lw(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: ro0lw not allocated!'
      allocate(qp0lw(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: qp0lw not allocated!'
      allocate(qs0lw(l0),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: qs0lw not allocated!'

      return
      end