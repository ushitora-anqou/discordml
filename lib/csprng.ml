let random_string len = Mirage_crypto_rng.generate len |> Cstruct.to_string

let random_2bytes_int () =
  let src = Mirage_crypto_rng.generate 2 in
  (Cstruct.get_byte src 0 * 256) + Cstruct.get_byte src 1

let random_4bytes_int () =
  let src = Mirage_crypto_rng.generate 4 in
  (Cstruct.get_byte src 0 * 256 * 256 * 256)
  + (Cstruct.get_byte src 1 * 256 * 256)
  + (Cstruct.get_byte src 2 * 256)
  + Cstruct.get_byte src 3
