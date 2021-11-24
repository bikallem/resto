(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (C) 2016, OCamlPro.                                             *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Serving a directory of registered services. *)

module type LOGGING = sig
  type +'a io

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_notice : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug : ('a, Format.formatter, unit, unit io) format4 -> 'a

  val lwt_log_info : ('a, Format.formatter, unit, unit io) format4 -> 'a

  val lwt_log_notice : ('a, Format.formatter, unit, unit io) format4 -> 'a

  val lwt_warn : ('a, Format.formatter, unit, unit io) format4 -> 'a

  val lwt_log_error : ('a, Format.formatter, unit, unit io) format4 -> 'a
end

module type IO = sig
  type +'a t

  type -'a u

  type input

  type 'a channel

  type output_channel

  val create : unit -> 'a t * 'a u

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val return : 'a -> 'a t

  val return_none : 'a option t

  val return_error : 'e -> ('a, 'e) result t

  val return_ok : 'a -> ('a, 'e) result t

  val return_some : 'a -> 'a option t

  module Stream : sig
    type 'a stream

    val from : (unit -> 'a option t) -> 'a stream

    val get : 'a stream -> 'a option t

    val junk_while : ('a -> bool) -> 'a stream -> unit t
  end

  module Body : sig
    type body = [Cohttp.Body.t | `Stream of string Stream.stream]

    val empty : body

    val is_empty : body -> bool t

    val to_string : body -> string t

    val of_string : string -> body

    val of_stream : string Stream.stream -> body

    val to_stream : body -> string Stream.stream
  end

  val resolve : 'a u -> 'a -> unit

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  module Server : sig
    type flow

    type conn = flow * Cohttp.Connection.t

    type response_action =
      [ `Expert of
        Cohttp.Response.t * (input channel -> output_channel -> unit t)
      | `Response of Cohttp.Response.t * Body.body ]

    type server

    val create :
      host:string ->
      stop:unit t ->
      mode:server ->
      on_exn:(exn -> unit) ->
      callback:(conn -> Cohttp.Request.t -> Body.body -> response_action t) ->
      conn_closed:(conn -> unit) ->
      unit t
  end

  val return_ok_response : 'a -> ([> `Response of 'a], 'e) result t

  val return_response : 'a -> [> `Response of 'a] t

  val return_unit : unit t

  val wseq : 'd channel -> output_channel -> (bytes * int * int) Seq.t -> unit t

  module Infix : sig
    val ( >>? ) :
      ('a, 'e) result -> ('a -> ('b, 'e) result t) -> ('b, 'e) result t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>=? ) :
      ('a, 'e) result t -> ('a -> ('b, 'e) result t) -> ('b, 'e) result t
  end
end
