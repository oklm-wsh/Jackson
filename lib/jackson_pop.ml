module Error = Jackson_error
module Decoder = Jackson_decoder
module Encoder = Jackson_encoder

let () = Printexc.record_backtrace true

module D =
struct
  open Jackson_baseDecoder

  let p_crlf p =
    p_chr '\r' @ p_chr '\n' @ p

  let is_atom = function
    | '\x01' .. '\x07' -> true
    | chr -> false

  let p_atom p state = p_while is_atom p state
  let p_space p state = p_chr ' ' p state

  type command =
    [ `Quit
    | `Stat
    | `List of string option
    | `Retr of string
    | `Dele of string
    | `Noop
    | `Rset
    | `User of string
    | `Pass of string ]

  let p_command p =
    p_atom
    @ fun command -> match String.uppercase command with
    | "QUIT" -> p_crlf (p `Quit)
    | "STAT" -> p_crlf (p `Stat)
    | "LIST" ->
        (p_space
         @ p_atom
         @ fun msg -> p_crlf
         @ fun state -> `Ok (msg, state))
         / (p_crlf @ p (`List None))
         @ fun msg -> p (`List (Some msg))
    | "RETR" ->
      p_space
      @ p_atom
      @ fun msg -> p_crlf
      @ p (`Retr msg)
    | "DELE" ->
      p_space
      @ p_atom
      @ fun msg -> p_crlf
      @ p (`Dele msg)
    | "NOOP" -> p_crlf (p `Noop)
    | "RSET" -> p_crlf (p `Rset)
    | c      -> p_crlf (p (`Unknow c))

  let p_to_crlf p state =
    let rec aux has_cr state =
      match peek_chr state with
      | Some '\n' when has_cr ->
        junk_chr state; p state
      | Some '\r' ->
        junk_chr state; aux true state
      | Some chr ->
        junk_chr state; aux false state
      | None -> read_line (aux has_cr) state
    in

    aux false state

  let rec p_response p state =
    match peek_chr state with
    | Some '+' -> junk_chr state; (p_str "OK" @ p_to_crlf @ p `Ok) state
    | Some '-' -> junk_chr state; (p_str "ERR" @ p_to_crlf  @ p `Err) state
    | Some chr -> raise (Error.Error (Error.err_unexpected chr state))
    | None -> read_line (p_response p) state

  let p_dot p state = p_chr '.' p state

  let p_body p state =
    let buf = Buffer.create 16 in

    let rec loop () =
      (p_crlf @ p_dot @ p_crlf @ fun state -> `Ok ((), state))
       / (fun state -> match peek_chr state with
          | Some chr -> Buffer.add_char buf chr; junk_chr state; loop () state
          | None -> read_line (fun state -> loop () state) state)
       @ (fun () -> p (Buffer.contents buf))
    in

    loop () state

  let is_digit = function
    | '0' .. '9' -> true
    | _          -> false

  let p_list p state =
    let rec loop acc =
      (p_while is_digit
       @ fun msg -> let msg = int_of_string msg in p_space
       @ p_while is_digit
       @ fun size -> let size = int_of_string size in p_crlf
       @ fun state -> `Ok ((msg, size), state))
      / (p_dot @ p_crlf @ p (List.rev acc))
      @ (fun data state -> loop (data :: acc) state)
    in

    read_line (loop []) state

  let p_noop p state =
    p state

  let p_err _ = `Err
  let p_ok _  = `Ok

  let extract_data p = function
    | `List None -> (p_list @ fun l -> p (`List l))
    | `Retr _    -> (p_body @ fun o -> p (`Body o))
    | _          -> p_ok

  let decode state x =
    let p_data = extract_data (fun x state -> x) x in
    (read_line
     @ p_response
     @ function
       | `Err -> p_err
       | `Ok -> p_data)
    state
end

module E =
struct
  open Jackson_baseEncoder

  let w_crlf k state =
    w "\r\n" k state

  let w_command = function
    | `Quit            -> w "QUIT" $ w_crlf
    | `Stat            -> w "STAT" $ w_crlf
    | `List (Some msg) -> w "LIST" & w (string_of_int msg) $ w_crlf
    | `List None       -> w "LIST" $ w_crlf
    | `Retr msg        -> w "RETR" & w (string_of_int msg) $ w_crlf
    | `Dele msg        -> w "DELE" & w (string_of_int msg) $ w_crlf
    | `Noop            -> w "NOOP" $ w_crlf
    | `Rset            -> w "RSET" $ w_crlf
    | `User user       -> w "USER" & w user $ w_crlf
    | `Pass pass       -> w "PASS" & w pass $ w_crlf

  let w_ret _ = `Ok

  let encode state cmd = w_command cmd (flush w_ret) state
end

type t =
  { dec : Decoder.t
  ; enc : Encoder.t
  ; mutable state : [ `Auth | `Trans ] }

let encode x k c =
  let rec loop = function
    | `Partial (s, i, l, k) ->
      `Write (s, i, l, (fun n -> loop (k n)))
    | `Ok -> k c
  in

  loop (E.encode c.enc x)

exception Invalid_command

let decode x k c =
  let rec loop = function
    | `Read (s, i, l, k) ->
      `Read (s, i, l, fun n -> loop (k n))
    | (`Ok | `Body _ | `List _ | `Err) as result -> k result
    | `Error _ as err ->
      raise (Error.Error err)
  in

  loop (D.decode c.dec x)

let connection () =
  let state =
    { dec = Decoder.make ()
    ; enc = Encoder.make ()
    ; state = `Auth }
  in

  state, decode `Conn (fun x -> x) state

let run state cmd =
  encode cmd (decode cmd (fun x -> x)) state
