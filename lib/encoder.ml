type t =
  { mutable buffer : Bytes.t
  ; mutable pos    : int }

let make () =
  { buffer = Bytes.create 4096
  ; pos    = 0 }
