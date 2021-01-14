(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt.Infix
open Resto_cohttp_server

module Make (Encoding : Resto.ENCODING) = struct
  module Directory = Resto_directory.Make (Encoding)
  module Media_type = Media_type.Make (Encoding)
  module Server = Server.Make (Encoding)

  type local_server = {
    root : unit Directory.directory;
    cors : Cors.t;
    medias : Server.Internal.medias;
    acl : Acl.t;
    agent : string;
  }

  let create (server : local_server) =
    ( module struct
      let uri_to_path uri =
        uri |> Uri.path |> Resto.Utils.split_path |> List.map Uri.pct_decode

      let ( >>=? ) = Lwt_result.( >>= )

      let ( >>? ) v f =
        match v with Error e -> Lwt.return_error e | Ok x -> f x

      let create_stream to_string s =
        Lwt_stream.from (fun () ->
            s.Resto_directory.Answer.next () >|= Option.map to_string)

      let call ~headers ?body meth uri path =
        Directory.lookup server.root () meth path
        >>=? fun (Directory.Service s) ->
        Server.Internal.input_media_type server.medias ~headers
        >>? fun input_media_type ->
        Server.Internal.output_content_media_type server.medias ~headers
        >>? fun (_output_content_type, output_media_type) ->
        ( match
            Resto.Query.parse
              s.types.query
              (List.map
                 (fun (k, l) -> (k, String.concat "," l))
                 (Uri.query uri))
          with
        | exception Resto.Query.Invalid s ->
            Lwt.return_error (`Cannot_parse_query s)
        | query -> (
            if not @@ Acl.allowed server.acl ~meth ~path then
              Lwt.return_ok (`Unauthorized None)
            else
              match s.types.input with
              | Directory.Service.No_input ->
                  s.handler query () >>= Lwt.return_ok
              | Directory.Service.Input input -> (
                  Cohttp_lwt.Body.to_string
                    (Option.value body ~default:Cohttp_lwt.Body.empty)
                  >>= fun body ->
                  match input_media_type.destruct input body with
                  | Error s ->
                      Lwt.return_error (`Cannot_parse_body s)
                  | Ok body ->
                      s.handler query body >>= Lwt.return_ok ) ) )
        >>= function
        | Ok answer -> (
            let output = output_media_type.construct s.types.output in
            let error = function
              | None ->
                  (Cohttp_lwt.Body.empty, Cohttp.Transfer.Fixed 0L)
              | Some e ->
                  let s = output_media_type.construct s.types.error e in
                  ( Cohttp_lwt.Body.of_string s,
                    Cohttp.Transfer.Fixed (Int64.of_int (String.length s)) )
            in
            match answer with
            | ( `Ok _
              | `Created _
              | `No_content
              | `Unauthorized _
              | `Forbidden _
              | `Gone _
              | `Not_found _
              | `Conflict _
              | `Error _ ) as a ->
                Server.Internal.handle_rpc_answer ~headers output error a
            | `OkStream o ->
                let body = create_stream output o in
                let encoding = Cohttp.Transfer.Chunked in
                Lwt.return_ok
                  ( Cohttp.Response.make ~status:`OK ~encoding ~headers (),
                    Cohttp_lwt.Body.of_stream body ) )
        | Error err ->
            Lwt.return_ok @@ Server.Internal.handle_error server.medias err

      let call ?headers ?body (meth : Cohttp.Code.meth) uri =
        let module Internal = Server.Internal in
        let path = uri |> Uri.path |> Resto.Utils.decode_split_path in
        let headers = Option.value headers ~default:(Cohttp.Header.init ()) in
        ( match meth with
        | #Resto.meth when Internal.invalid_cors server.cors headers ->
            Internal.invalid_cors_response server.agent
        | #Resto.meth as meth ->
            call ~headers ?body meth uri path
        | `OPTIONS ->
            Internal.handle_options server.root server.cors headers path
        | _ ->
            Lwt.return_error `Not_implemented )
        >>= function
        | Ok a ->
            Lwt.return a
        | Error err ->
            Lwt.return @@ Server.Internal.handle_error server.medias err
    end : Client.CALL )

  let launch ?(cors = Cors.default) ?(agent = Server.Internal.default_agent)
      ?(acl = Acl.Allow_all {except = []}) ~media_types root =
    let default_media_type = Server.Internal.default_media_type media_types in
    let medias : Server.Internal.medias = {media_types; default_media_type} in
    let local_server = {root; cors; medias; agent; acl} in
    create local_server
end