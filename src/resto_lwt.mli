module Io :
  Resto.IO
    with type +'a t = 'a Lwt.t
    with type -'a u = 'a Lwt.u
    with type input = Lwt_io.input
    with type 'a channel = 'a Lwt_io.channel
    with type output_channel = Lwt_io.output_channel
    with type 'a Stream.stream = 'a Lwt_stream.t
    with type Server.flow = Conduit_lwt_unix.flow
    with type Server.server = Conduit_lwt_unix.server
