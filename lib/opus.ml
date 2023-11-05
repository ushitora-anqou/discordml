module Low_level = struct
  open Ctypes
  open Foreign

  type encoder = unit ptr

  let encoder : encoder typ = ptr void
  let int16 = short
  let int32 = int

  let encoder_create =
    foreign "opus_encoder_create"
      (int32 @-> int @-> int @-> ptr int @-> returning encoder)

  let encode =
    foreign "opus_encode"
      (encoder @-> ptr int16 @-> int @-> ptr char @-> int32 @-> returning int32)

  let encoder_destroy =
    foreign "opus_encoder_destroy" (encoder @-> returning void)
end

type application = Voip | Audio | RestrictedLowdelay

module Encoder = struct
  type t = { encoder : Low_level.encoder; sample_rate : int; channels : int }

  let create ~sample_rate ~channels ~application =
    if sample_rate <> 48000 || channels <> 2 then
      failwith "Only 48kHz stereo is supported";

    let open Ctypes in
    let application =
      match application with
      | Voip -> 2048
      | Audio -> 2049
      | RestrictedLowdelay -> 2051
    in
    let error = allocate int 0 in
    let encoder =
      Low_level.encoder_create sample_rate channels application error
    in
    if !@error <> 0 then
      failwith (Printf.sprintf "Failed to create Opus encoder: %d" !@error);
    { encoder; sample_rate; channels }

  let destroy { encoder; _ } = Low_level.encoder_destroy encoder

  let encode ?(frame_size = 960) encoder (`S16LE pcm_s16le) =
    if Sys.big_endian then failwith "Big endian machine is not supported";

    let pcm_size_in_bytes = frame_size * encoder.channels * 2 in
    if String.length pcm_s16le <> pcm_size_in_bytes then
      failwith "Invalid frame and pcm size";

    let open Ctypes in
    let pcm =
      pcm_s16le |> CArray.of_string |> CArray.start |> to_voidp
      |> from_voidp short
    in
    let data = allocate_n char ~count:pcm_size_in_bytes in
    let data_size =
      Low_level.encode encoder.encoder pcm frame_size data pcm_size_in_bytes
    in
    if data_size < 0 then failwith "opus encode failure";
    data |> string_from_ptr ~length:data_size |> Bytes.of_string
end
