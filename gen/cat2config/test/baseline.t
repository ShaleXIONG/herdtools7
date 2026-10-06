aarch64.cat
  $ mcat2config7 --set-libdir ./libdir --let ca libdir/aarch64.cat
  Fr
  Co
  $ mcat2config7 --set-libdir ./libdir --let lrs libdir/aarch64.cat
  PosWR
  $ mcat2config7 --set-libdir ./libdir --let Exp-haz-ob libdir/aarch64.cat
  [PosRR,Fre]
  $ mcat2config7 --set-libdir ./libdir --let haz-ob libdir/aarch64.cat
  [PosRR,Fre]
  $ mcat2config7 --set-libdir ./libdir --let Exp-obs libdir/aarch64.cat
  Rfe
  Fre
  Coe
aarch64hwreqs.cat
  $ mcat2config7 --set-libdir ./libdir --let DSB-ob libdir/aarch64.cat
  [ExpObs,DSB.SY***]
  DSB.LD*R*
  DSB.ST*W*
  $ mcat2config7 --set-libdir ./libdir --let IFB-ob libdir/aarch64.cat
  [DpCtrl,ISB,@after([ExpObs|Hat])]
  [DpCtrlCsel,ISB,@after([ExpObs|Hat])]
  [DpAddr,ISB***,@after([ExpObs|Hat])]
  [DpAddrCsel,ISB***,@after([ExpObs|Hat])]
  [ExpObs,DSB.SY***,ISB,@after([ExpObs|Hat])]
  [DSB.LD*R*,ISB,@after([ExpObs|Hat])]
  [DSB.ST*W*,ISB,@after([ExpObs|Hat])]
  $ mcat2config7 --set-libdir ./libdir --let dob libdir/aarch64.cat
  [DpAddr,@after([ExpObs|Hat])]
  DpData*W
  DpCtrl*W
  [DpAddr,Po**W]
  [DpAddr*W,PosWR]
  [DpData*W,PosWR]
  $ mcat2config7 --set-libdir ./libdir --let pob libdir/aarch64.cat
  DpAddrCsel*W
  DpDataCsel
  DpCtrlCsel*W
  [DpAddrCsel,Po**W]
  $ mcat2config7 --set-libdir ./libdir --let aob libdir/aarch64.cat
  LxSx
  Amo.Safe
  [Hat,LxSx]
  [Hat,Amo.Safe]
  [LxSx,PosWR,A]
  [Amo.Safe,PosWR,A]
  [Hat,LxSx,PosWR,A]
  [Hat,Amo.Safe,PosWR,A]
  [LxSx,PosWR,A,LxSx]
  [Amo.Safe,PosWR,A,LxSx]
  [LxSx,PosWR,A,Amo.Safe]
  [Amo.Safe,PosWR,A,Amo.Safe]
  [Hat,LxSx,PosWR,A,LxSx]
  [Hat,Amo.Safe,PosWR,A,LxSx]
  [Hat,LxSx,PosWR,A,Amo.Safe]
  [Hat,Amo.Safe,PosWR,A,Amo.Safe]
  [LxSx,PosWR,Q]
  [Amo.Safe,PosWR,Q]
  [Hat,LxSx,PosWR,Q]
  [Hat,Amo.Safe,PosWR,Q]
  [LxSx,PosWR,Q,LxSx]
  [Amo.Safe,PosWR,Q,LxSx]
  [LxSx,PosWR,Q,Amo.Safe]
  [Amo.Safe,PosWR,Q,Amo.Safe]
  [Hat,LxSx,PosWR,Q,LxSx]
  [Hat,Amo.Safe,PosWR,Q,LxSx]
  [Hat,LxSx,PosWR,Q,Amo.Safe]
  [Hat,Amo.Safe,PosWR,Q,Amo.Safe]
  $ mcat2config7 --set-libdir ./libdir --let bob libdir/aarch64.cat
  [ExpObs,DMB.SY***,@after([ExpObs|Hat])]
  [DMB.LD*R*,@after([ExpObs|Hat])]
  DMB.ST*WW
  [A,Amo.Safe,L,Po,@after([ExpObs|Hat])]
  [ExpObs,Po,A,Amo.Safe,L,Po,@after([ExpObs|Hat])]
  [L,Po,A]
  [L,Po,A,LxSx]
  [L,Po,A,Amo.Safe]
  [LxSx,L,Po,A]
  [Amo.Safe,L,Po,A]
  [Hat,LxSx,L,Po,A]
  [Hat,Amo.Safe,L,Po,A]
  [LxSx,L,Po,A,LxSx]
  [Amo.Safe,L,Po,A,LxSx]
  [LxSx,L,Po,A,Amo.Safe]
  [Amo.Safe,L,Po,A,Amo.Safe]
  [Hat,LxSx,L,Po,A,LxSx]
  [Hat,Amo.Safe,L,Po,A,LxSx]
  [Hat,LxSx,L,Po,A,Amo.Safe]
  [Hat,Amo.Safe,L,Po,A,Amo.Safe]
  [A,Po,@after([ExpObs|Hat])]
  [A,LxSx,Po,@after([ExpObs|Hat])]
  [A,Amo.Safe,Po,@after([ExpObs|Hat])]
  [Hat,A,LxSx,Po,@after([ExpObs|Hat])]
  [Hat,A,Amo.Safe,Po,@after([ExpObs|Hat])]
  [Q,Po,@after([ExpObs|Hat])]
  [Q,LxSx,Po,@after([ExpObs|Hat])]
  [Q,Amo.Safe,Po,@after([ExpObs|Hat])]
  [Hat,Q,LxSx,Po,@after([ExpObs|Hat])]
  [Hat,Q,Amo.Safe,Po,@after([ExpObs|Hat])]
  [ExpObs,Po,L]
  [ExpObs,Po,LxSx,L]
  [ExpObs,Po,Amo.Safe,L]
aarch64deps.cat
  $ mcat2config7 --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  Pos*W
Pure unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let pick-lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let hw-reqs libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let obs libdir/aarch64.cat
Recursive unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let ob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let local-hw-reqs libdir/aarch64.cat
