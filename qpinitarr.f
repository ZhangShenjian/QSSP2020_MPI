      subroutine  qpinitarr(ierr)
      use qpalloc
      implicit none

      integer*4 ierr
c
      allocate(plm(0:ldegmax,0:2),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: plm not allocated!'
c
      allocate(ul0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ul0 not allocated!'
      allocate(vl0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: vl0 not allocated!'
      allocate(wl0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: wl0 not allocated!'
      allocate(el0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: el0 not allocated!'
      allocate(fl0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: fl0 not allocated!'
      allocate(gl0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: gl0 not allocated!'
c
      allocate(pl0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: pl0 not allocated!'
      allocate(ql0(0:ldegmax,6),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ql0 not allocated!'
c
      allocate(urlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: urlm not allocated!'
      allocate(utlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: utlm not allocated!'
      allocate(uplm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: uplm not allocated!'
c
      allocate(grlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: grlm not allocated!'
      allocate(gtlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: gtlm not allocated!'
      allocate(gplm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: gplm not allocated!'
c
      allocate(errlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: errlm not allocated!'
      allocate(ertlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ertlm not allocated!'
      allocate(erplm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: erplm not allocated!'
      allocate(etrlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: etrlm not allocated!'
      allocate(ett0lm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ett0lm not allocated!'
      allocate(ettalm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ettalm not allocated!'
      allocate(ettblm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ettblm not allocated!'
      allocate(etp0lm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: etp0lm not allocated!'
      allocate(etpalm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: etpalm not allocated!'
      allocate(etpblm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: etpblm not allocated!'
      allocate(eprlm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: eprlm not allocated!'
      allocate(ept0lm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: ept0lm not allocated!'
      allocate(eptalm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: eptalm not allocated!'
      allocate(eptblm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: eptblm not allocated!'
      allocate(epp0lm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: epp0lm not allocated!'
      allocate(eppalm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: eppalm not allocated!'
      allocate(eppblm(0:ldegmax,6,0:ndmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgetinp: eppblm not allocated!'
c
      allocate(lyupp(0:ldegmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lyupp not allocated!'
      allocate(lyups(0:ldegmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lyups not allocated!'
      allocate(lyupt(0:ldegmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lyupt not allocated!'
      allocate(lylwp(0:ldegmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lylwp not allocated!'
      allocate(lylws(0:ldegmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lylws not allocated!'
      allocate(lylwt(0:ldegmax),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lylwt not allocated!'
c
      allocate(lygrn(ngrn),stat=ierr)
      if(ierr.ne.0)stop ' Error in qpgettinp: lygrn not allocated!'
c
      return
      end