(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                            *)
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

module Make
    (Encoding : Resto.ENCODING)
    (Io : Resto.IO)
    (Log : Resto.LOGGING with type +'a io = 'a Io.t) : sig
  module Media_type : module type of struct
    include Media_type.Make (Encoding)
  end

  module Directory : module type of struct
    include Resto_directory.Make (Encoding) (Io)
  end

  (** A handle on the server worker. *)
  type server

  (** Promise a running RPC server.*)
  val launch :
    ?host:string ->
    ?cors:Cors.t ->
    ?agent:string ->
    ?acl:Acl.t ->
    media_types:Media_type.t list ->
    Io.Server.server ->
    unit Directory.t ->
    server Io.t

  (* configure the access list for this server *)
  val set_acl : server -> Acl.t -> unit

  (** Kill an RPC server. *)
  val shutdown : server -> unit Io.t
end

(** [Make_selfserver] is a functor that produces only the machinery necessary
    for local use. Specifically, it produces the values and types needed for the
    [Self_serving_client]. *)
module Make_selfserver
    (Encoding : Resto.ENCODING)
    (Io : Resto.IO)
    (Log : Resto.LOGGING with type +'a io = 'a Io.t) : sig
  module Media_type : module type of struct
    include Media_type.Make (Encoding)
  end

  module Directory : module type of struct
    include Resto_directory.Make (Encoding) (Io)
  end

  module Media : sig
    type medias = {
      media_types: Media_type.t list;
      default_media_type: string * Media_type.t;
    }

    val default_media_type : Media_type.t list -> string * Media_type.t

    val input_media_type :
      ?headers:Cohttp.Header.t ->
      medias ->
      (Media_type.t, [> `Unsupported_media_type of string]) result

    val output_content_media_type :
      ?headers:Cohttp.Header.t ->
      medias ->
      (string * Media_type.t, [> `Not_acceptable]) result
  end

  module Agent : sig
    val default_agent : string
  end

  module Handlers : sig
    val invalid_cors : Resto_cohttp.Cors.t -> Cohttp.Header.t -> bool

    val invalid_cors_response : string -> Cohttp.Response.t * Io.Body.body

    val handle_error :
      Cohttp.Header.t ->
      Media.medias ->
      [< `Cannot_parse_body of string
      | `Cannot_parse_path of string list * Resto.Arg.descr * string
      | `Cannot_parse_query of string
      | `Method_not_allowed of [< Resto.meth] list
      | `Not_acceptable
      | `Not_found
      | `Not_implemented
      | `Unsupported_media_type of 'a ] ->
      Cohttp.Response.t * Io.Body.body

    val handle_rpc_answer :
      string ->
      ?headers:(* connection identifier for logging *)
               Cohttp.Header.t ->
      ('o -> string) ->
      [< `Created of string option | `No_content | `Ok of 'o] ->
      Cohttp.Response.t * Io.Body.body

    val handle_rpc_answer_error :
      string ->
      ?headers:(* connection identifier for logging *)
               Cohttp.Header.t ->
      ('e -> Io.Body.body * Cohttp.Transfer.encoding) ->
      [< `Conflict of 'e
      | `Error of 'e
      | `Forbidden of 'e
      | `Gone of 'e
      | `Not_found of 'e
      | `Unauthorized of 'e ] ->
      Cohttp.Response.t * Io.Body.body

    val handle_rpc_answer_chunk :
      ?headers:Cohttp.Header.t ->
      ('o -> (bytes * int * int) Seq.t) ->
      [< `OkChunk of 'o] ->
      Cohttp.Response.t * ('d Io.channel -> Io.output_channel -> unit Io.t)

    val handle_options :
      unit Directory.t ->
      Resto_cohttp.Cors.t ->
      Cohttp.Header.t ->
      string list ->
      (Cohttp.Response.t * Io.Body.body, [> Directory.lookup_error]) result Io.t
  end
end
