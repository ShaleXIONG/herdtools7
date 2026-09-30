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
  [Fre|Coe]
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
  [[DSB.LD*R*|DSB.ST*W*],ISB,@after([ExpObs|Hat])]
  [ExpObs,DSB.SY***,ISB,@after([ExpObs|Hat])]
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
  [Hat?,[LxSx|Amo.Safe]]
  [Hat?,[LxSx|Amo.Safe],PosWR,[A|Q],[LxSx|Amo.Safe]?]
  $ mcat2config7 --set-libdir ./libdir --let bob libdir/aarch64.cat
  [ExpObs,DMB.SY***,@after([ExpObs|Hat])]
  [DMB.LD*R*,@after([ExpObs|Hat])]
  DMB.ST*WW
  [A,Amo.Safe,L,Po,@after([ExpObs|Hat])]
  [ExpObs,Po,A,Amo.Safe,L,Po,@after([ExpObs|Hat])]
  [Hat?,[LxSx|Amo.Safe],L,Po,A,[LxSx|Amo.Safe]?]
  [L,Po,A,[LxSx|Amo.Safe]?]
  [Hat?,[A|Q],[LxSx|Amo.Safe],Po,@after([ExpObs|Hat])]
  [[A|Q],Po,@after([ExpObs|Hat])]
  [ExpObs,Po,[LxSx|Amo.Safe]?,L]
aarch64deps.cat
  $ mcat2config7 --set-libdir ./libdir --let lwfs libdir/aarch64.cat
  Pos*W
Pos*W pruning
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let DSB-ob libdir/aarch64.cat
  [ExpObs,[DSB.SY**R|DSB.SYd*W]]
  [DSB.LDdRW|DSB.LD*RR]
  [DSB.STdWW|DSB.ST*WR]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let IFB-ob libdir/aarch64.cat
  [[DpCtrldW|DpCtrl*R],ISB,@after([ExpObs|Hat])]
  [[DpCtrlCseldW|DpCtrlCsel*R],ISB,@after([ExpObs|Hat])]
  [[DpAddrdW|DpAddr*R],[ISB**R|ISBd*W],@after([ExpObs|Hat])]
  [[DpAddrCseldW|DpAddrCsel*R],[ISB**R|ISBd*W],@after([ExpObs|Hat])]
  [[DSB.LDdRW|DSB.STdWW|DSB.LD*RR|DSB.ST*WR],ISB,@after([ExpObs|Hat])]
  [ExpObs,[DSB.SY**R|DSB.SYd*W],ISB,@after([ExpObs|Hat])]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let dob libdir/aarch64.cat
  [[DpAddrdW|DpAddr*R],@after([ExpObs|Hat])]
  DpDatadW
  DpCtrldW
  [[DpAddrdW|DpAddr*R],Pod*W]
  [DpAddrdW,PosWR]
  [DpDatadW,PosWR]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let pob libdir/aarch64.cat
  DpAddrCseldW
  [DpDataCseldW|DpDataCsel*R]
  DpCtrlCseldW
  [[DpAddrCseldW|DpAddrCsel*R],Pod*W]
  $ mcat2config7 -prune pos_w --set-libdir ./libdir --let bob libdir/aarch64.cat
  [ExpObs,[DMB.SY**R|DMB.SYd*W],@after([ExpObs|Hat])]
  [[DMB.LDdRW|DMB.LD*RR],@after([ExpObs|Hat])]
  DMB.STdWW
  [A,Amo.Safe,L,[Po**R|Pod*W],@after([ExpObs|Hat])]
  [ExpObs,[Po**R|Pod*W],A,Amo.Safe,L,[Po**R|Pod*W],@after([ExpObs|Hat])]
  [Hat?,[LxSx|Amo.Safe],L,[Po**R|Pod*W],A,[LxSx|Amo.Safe]?]
  [L,[Po**R|Pod*W],A,[LxSx|Amo.Safe]?]
  [Hat?,[A|Q],[LxSx|Amo.Safe],[Po**R|Pod*W],@after([ExpObs|Hat])]
  [[A|Q],[Po**R|Pod*W],@after([ExpObs|Hat])]
  [ExpObs,[Po**R|Pod*W],[LxSx|Amo.Safe]?,L]
Pure unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let pick-lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let hw-reqs libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let obs libdir/aarch64.cat
Recursive unions (temporarily disabled as their output is very large)
$ mcat2config7 --set-libdir ./libdir --let ob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let lob libdir/aarch64.cat
$ mcat2config7 --set-libdir ./libdir --let local-hw-reqs libdir/aarch64.cat
