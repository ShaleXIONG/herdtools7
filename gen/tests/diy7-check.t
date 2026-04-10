An explicit comma stays a sequence after a choice in `diy7`
  $ diy7 -arch AArch64 -mode free -filter-check 'PodWR|Rfe,Fre' PodWR
  Sequence `[PodWR,Fre]` `PodWR` passes the internal filter in mode `free`
  Sequence `[Rfe,Fre]` `PodWR` passes the internal filter in mode `free`

`PPO` unfolds to concrete PPC relaxations in `diy7`
  $ diy7 -arch PPC -mode free -filter-check PPO DpDatadW
  Sequence `[DpDatadW,PosWR,DpCtrldW]` `DpDatadW` is prohibited in the internal filter in mode `free`
  Sequence `[DpDatadW,PosWR,DpCtrlIsyncdR]` `DpDatadW` passes the internal filter in mode `free`
  Sequence `[DpDatadW,PosWR,DpDatadW]` `DpDatadW` is prohibited in the internal filter in mode `free`
  Sequence `[DpDatadW,PosWR,DpAddrdR]` `DpDatadW` passes the internal filter in mode `free`
  Sequence `[DpDatadW,PosWR]` `DpDatadW` passes the internal filter in mode `free`
  Sequence `[DpAddrdR,DpDatadW]` `DpDatadW` is prohibited in the internal filter in mode `free`
  Sequence `[DpAddrdR,DpAddrdR]` `DpDatadW` passes the internal filter in mode `free`
  Sequence `[DpAddrdR,DpCtrldW]` `DpDatadW` is prohibited in the internal filter in mode `free`
  Sequence `[DpAddrdR,DpCtrlIsyncdR]` `DpDatadW` passes the internal filter in mode `free`
  Sequence `DpCtrlIsyncdR` `DpDatadW` passes the internal filter in mode `free`
  Sequence `DpCtrldW` `DpDatadW` is prohibited in the internal filter in mode `free`
  Sequence `DpAddrdR` `DpDatadW` passes the internal filter in mode `free`
  Sequence `DpDatadW` `DpDatadW` is prohibited in the internal filter in mode `free`

A `BC` relax macro is looked up before raw edge parsing
  $ diy7 -arch PPC -mode free -filter-check BCDpDatadW DpDatadW
  Sequence `[DpDatadW,Rfe]` `DpDatadW` passes the internal filter in mode `free`

An `AC` relax macro is looked up before raw edge parsing
  $ diy7 -arch AArch64 -mode free -filter-check ACDMB.SYdRW PodWR
  Sequence `[Rfe,DMB.SYdRW]` `PodWR` passes the internal filter in mode `free`

An `ABC` relax macro is looked up before raw edge parsing
  $ diy7 -arch AArch64 -mode free -filter-check ABCDMB.SYdRW PodWR
  Sequence `[Rfe,DMB.SYdRW,Rfe]` `PodWR` is prohibited in the internal filter in mode `free`

`allRW` expands through the named relax lookup table
  $ diy7 -arch AArch64 -mode free -filter-check allRW PodWR
  Sequence `PodRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpAddrdW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpAddrCseldW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpDatadW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpDataCseldW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpCtrldW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpCtrlCseldW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpCtrlIsbdW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpCtrlIsbCseldW` `PodWR` passes the internal filter in mode `free`
  Sequence `ISBdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.NSHLDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.NSHLDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.NSHSTdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.NSHSTdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.NSHdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.NSHdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.ISHLDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.ISHLDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.ISHSTdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.ISHSTdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.ISHdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.ISHdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.OSHLDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.OSHLDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.OSHSTdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.OSHSTdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.OSHdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.OSHdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.LDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.LDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.STdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.STdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.SYdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DSB.SYdRW` `PodWR` passes the internal filter in mode `free`

`someRW` expands through the named relax lookup table
  $ diy7 -arch AArch64 -mode free -filter-check someRW PodWR
  Sequence `PodRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DpDatadW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.LDdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.STdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.SYdRW` `PodWR` passes the internal filter in mode `free`
  Sequence `ISBdRW` `PodWR` passes the internal filter in mode `free`

`allWR` expands through the named relax lookup table
  $ diy7 -arch AArch64 -mode free -filter-check allWR PodWR
  Sequence `PodWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `ISBdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.NSHLDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.NSHLDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.NSHSTdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.NSHSTdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.NSHdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.NSHdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.ISHLDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.ISHLDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.ISHSTdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.ISHSTdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.ISHdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.ISHdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.OSHLDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.OSHLDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.OSHSTdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.OSHSTdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.OSHdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.OSHdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.LDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.LDdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.STdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.STdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DMB.SYdWR` `PodWR` is prohibited in the internal filter in mode `free`
  Sequence `DSB.SYdWR` `PodWR` is prohibited in the internal filter in mode `free`

`someWW` expands through the named relax lookup table
  $ diy7 -arch AArch64 -mode free -filter-check someWW PodWR
  Sequence `PodWW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.LDdWW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.STdWW` `PodWR` passes the internal filter in mode `free`
  Sequence `DMB.SYdWW` `PodWR` passes the internal filter in mode `free`
  Sequence `ISBdWW` `PodWR` passes the internal filter in mode `free`
