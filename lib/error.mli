type error =
  [ `Unexpected_eoi
  | `Expected_char       of char
  | `Expected_set        of char list
  | `Unexpected_char     of char
  | `Unexpected_str      of string
  | `Expected_str        of string ]

type     err = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

exception Error of err

val err                     : error -> Decoder.t -> err
val err_unexpected_eoi      : Decoder.t -> err
val err_expected            : char -> Decoder.t -> err
val err_expected_set        : char list -> Decoder.t -> err
val err_unexpected          : char -> Decoder.t -> err
val err_unexpected_str      : string -> Decoder.t -> err
val err_expected_str        : string -> Decoder.t -> err

val pp : Format.formatter -> error -> unit
