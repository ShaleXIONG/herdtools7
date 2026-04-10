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
