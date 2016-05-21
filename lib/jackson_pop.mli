module Error : (module type of Jackson_error)

type t

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
