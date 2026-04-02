(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute this software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Queue = struct
  exception Closed

  type 'a t =
    {
      q : 'a Stdlib.Queue.t;
      mutex : Mutex.t;
      not_empty : Condition.t;
      mutable closed : bool;
    }

  let create () =
    {
      q = Stdlib.Queue.create () ;
      mutex = Mutex.create () ;
      not_empty = Condition.create () ;
      closed = false;
    }

  let with_lock t f =
    Mutex.lock t.mutex ;
    try
      let r = f () in
      Mutex.unlock t.mutex ;
      r
    with e ->
      Mutex.unlock t.mutex ;
      raise e

  let close t =
    with_lock t (fun () ->
      if not t.closed then begin
        t.closed <- true ;
        Condition.broadcast t.not_empty
      end)

  let push t x =
    with_lock t (fun () ->
      if t.closed then raise Closed ;
      Stdlib.Queue.add x t.q ;
      Condition.signal t.not_empty)

  let pop t =
    with_lock t (fun () ->
      while Stdlib.Queue.is_empty t.q && not t.closed do
        Condition.wait t.not_empty t.mutex
      done ;
      if Stdlib.Queue.is_empty t.q then None
      else Some (Stdlib.Queue.take t.q))
end

type ('a,'b) t =
  {
    q : 'a Queue.t;
    thread : Thread.t;
    result : 'b option ref;
    failed : (exn * Printexc.raw_backtrace) option ref;
  }

let create apply initial_acc =
  let result = ref None
  and failed = ref None
  and q = Queue.create () in
  let rec loop acc =
    match Queue.pop q with
    | Some x -> loop (apply x acc)
    | None -> acc in
  let thread =
    Thread.create
      (fun () ->
        try result := Some (loop initial_acc)
        with e ->
          failed := Some (e,Printexc.get_raw_backtrace ()))
      () in
  { q; thread; result; failed; }

let push t x =
  Queue.push t.q x

let close t =
  Queue.close t.q

let join t =
  Thread.join t.thread ;
  match !(t.failed),!(t.result) with
  | Some (e,bt),_ -> Printexc.raise_with_backtrace e bt
  | None,Some r -> r
  | None,None -> assert false

let finish t =
  close t ;
  join t
