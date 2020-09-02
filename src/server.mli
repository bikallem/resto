(**************************************************************************)
(*  resto                                                                 *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Serving a directory of registered services. *)

module type LOGGING = sig
  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_notice : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

  val log_error : ('a, Format.formatter, unit, unit) format4 -> 'a

  val lwt_debug : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_info : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_notice : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_warn : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

  val lwt_log_error : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
end

module Make (Encoding : Resto.ENCODING) (Log : LOGGING) : sig
  (** A handle on the server worker. *)
  type server

  (** Promise a running RPC server.*)
  val launch :
    ?host:string ->
    ?cors:Cors.t ->
    ?acl:Acl.t ->
    media_types:Media_type.Make(Encoding).t list ->
    Conduit_lwt_unix.server ->
    unit Resto_directory.Make(Encoding).t ->
    server Lwt.t

  (* configure the access list for this server *)
  val set_acl : server -> Acl.t -> unit

  (** Kill an RPC server. *)
  val shutdown : server -> unit Lwt.t
end
