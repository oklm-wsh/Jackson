type error =
  [ `Unexpected_eoi
  | `Expected_char       of char
  | `Expected_set        of char list
  | `Unexpected_char     of char
  | `Unexpected_str      of string
  | `Expected_str        of string ]

let err e state                       = `Error
                                        (e, state.Decoder.buffer,
                                            state.Decoder.pos,
                                            state.Decoder.len)
let err_unexpected_eoi state          = err `Unexpected_eoi state
let err_expected chr state            = err (`Expected_char chr) state
let err_expected_set set state        = err (`Expected_set set) state
let err_unexpected chr state          = err (`Unexpected_char chr) state
let err_unexpected_str str state      = err (`Unexpected_str str) state
let err_expected_str str state        = err (`Expected_str str) state
let p = Format.fprintf

let pp_lst ?(sep = "") pp_data fmt set =
  let rec aux = function
    | [] -> ()
    | [ x ] -> p fmt "%a" pp_data x
    | x :: r -> p fmt "%a%s" pp_data x sep; aux r
  in
  aux set

let pp_char fmt = p fmt "%c"

let pp fmt = function
  | `Unexpected_eoi          -> p fmt "Unexpected EOI"
  | `Expected_char chr       -> p fmt "Expected [%S]" (String.make 1 chr)
  | `Expected_set set        -> p fmt "Expected [%a]"
                                  (pp_lst ~sep:" | " pp_char) set
  | `Unexpected_char chr     -> p fmt "Unexpected [%S]" (String.make 1 chr)
  | `Unexpected_str str      -> p fmt "Unexpected [%S]" str
  | `Expected_str str        -> p fmt "Expected [%S]" str

type     err = [ `Error of error * string * int * int ]
type 'a read = [ `Read of Bytes.t * int * int * (int -> 'a) ]

let pp_error fmt = function
  | `Error (err, buffer, pos, len) ->
    p fmt "Error: %a" pp err

exception Error of err

let () = Printexc.register_printer
  (function Error err ->
     let buf = Buffer.create 16 in
     let fmt = Format.formatter_of_buffer buf in
     Format.fprintf fmt "%a%!" pp_error err;
     Some (Buffer.contents buf)
   | _ -> None)
