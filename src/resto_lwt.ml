module Io = struct
  type +'a t = 'a Lwt.t

  type -'a u = 'a Lwt.u

  type input = Lwt_io.input

  type 'a channel = 'a Lwt_io.channel

  type output_channel = Lwt_io.output_channel

  let create = Lwt.wait

  let bind = Lwt.bind

  let map t f = Lwt.map f t

  let return = Lwt.return

  let return_none = Lwt.return_none

  let return_error = Lwt.return_error

  let return_ok = Lwt.return_ok

  let return_some = Lwt.return_some

  module Stream = struct
    type 'a stream = 'a Lwt_stream.t

    let from = Lwt_stream.from

    let get = Lwt_stream.get

    let junk_while = Lwt_stream.junk_while
  end

  module Body = struct
    type body = [Cohttp.Body.t | `Stream of string Stream.stream]

    let empty = Cohttp_lwt.Body.empty

    let is_empty = Cohttp_lwt.Body.is_empty

    let to_string = Cohttp_lwt.Body.to_string

    let of_string = Cohttp_lwt.Body.of_string

    let of_stream = Cohttp_lwt.Body.of_stream

    let to_stream = Cohttp_lwt.Body.to_stream
  end

  let resolve = Lwt.wakeup_later

  let catch = Lwt.catch

  open Lwt.Infix

  module Server = struct
    type flow = Conduit_lwt_unix.flow

    type conn = flow * Cohttp.Connection.t

    type response_action =
      [ `Expert of
        Cohttp.Response.t * (input channel -> output_channel -> unit t)
      | `Response of Cohttp.Response.t * Body.body ]

    type server = Conduit_lwt_unix.server

    let create ~host ~stop ~mode ~on_exn ~callback ~conn_closed =
      Conduit_lwt_unix.init ~src:host () >>= fun ctx ->
      let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
      Cohttp_lwt_unix.Server.create
        ~ctx
        ~stop
        ~mode
        ~on_exn
        (Cohttp_lwt_unix.Server.make_response_action ~callback ~conn_closed ())
  end

  let return_ok_response r = return_ok (`Response r)

  let return_response r = return (`Response r)

  let return_unit = return ()

  let wchunk oc (item, offset, length) =
    if length = 0 then Lwt.return_unit
    else
      Lwt_io.fprintf oc "%X\r\n" length >>= fun () ->
      Lwt_io.write_from_exactly oc item offset length >>= fun () ->
      Lwt_io.write_from_string_exactly oc "\r\n" 0 2 >>= fun () ->
      Lwt_io.flush oc

  let rec drain seq =
    match seq () with
    | Seq.Nil -> Lwt.return_unit
    | Seq.Cons (_, seq) ->
        Lwt.pause () >>= fun () -> (drain [@ocaml.tailcall]) seq

  let rec wseq ic oc seq =
    match seq () with
    | Seq.Nil ->
        Lwt_io.write_from_string_exactly oc "0\r\n\r\n" 0 5 >>= fun () ->
        Lwt_io.flush oc
    | Seq.Cons (chunk, seq) ->
        Lwt.try_bind
          (fun () -> wchunk oc chunk)
          (fun () ->
            Lwt.pause () >>= fun () -> (wseq [@ocaml.tailcall]) ic oc seq)
          (fun exc -> drain seq >>= fun () -> raise exc)

  let wseq ic oc seq =
    Lwt.finalize (fun () -> wseq ic oc seq) (fun () -> Lwt_io.close ic)

  module Infix = struct
    let ( >>? ) v f = match v with Ok x -> f x | Error err -> return_error err

    let ( >|= ) = map

    let ( >>= ) = bind

    let ( >>=? ) e f =
      bind e (function Error e -> return (Error e) | Ok x -> f x)
  end
end
