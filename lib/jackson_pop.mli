module Error : (module type of Jackson_error)
module Decoder : (module type of Jackson_decoder)
module Encoder : (module type of Jackson_encoder)

type t =
  { dec : Decoder.t
  ; enc : Encoder.t
  ; mutable state : [ `Auth | `Trans ] }

val connection : unit ->
  t * ([> `Body of string
        | `Err
        | `List of (int * int) list
        | `Ok
        | `Read of Bytes.t * int * int * (int -> 'a) ]
       as 'a)

val run : t ->
  [< `Dele of int
   | `List of int option
   | `Noop
   | `Pass of string
   | `Quit
   | `Retr of int
   | `Rset
   | `Stat
   | `User of string
   > `List `Retr ] ->
  ([> `Body of string
    | `Err
    | `List of (int * int) list
    | `Ok
    | `Read of Bytes.t * int * int * (int -> 'a)
    | `Write of Bytes.t * int * int * (int -> 'a) ]
   as 'a)
