open BaseDecoder
open BaseEncoder

let p_crlf p state =
  p_chr '\r' state
  ; p_chr '\n' state
  ; p state

let is_atom = function
  | '\x01' .. '\x07' -> true
  | chr -> false

let p_atom state = p_while is_atom state
let p_space = p_chr ' '

type command =
  [ `Quit
  | `Stat
  | `List of string option
  | `Retr of string
  | `Dele of string
  | `Noop
  | `Rset ]

let p_command p state =
  let command = p_atom state in

  match String.uppercase command with
  | "QUIT" -> p_crlf (p `Quit) state
  | "STAT" -> p_crlf (p `Stat) state
  | "LIST" ->
      ((fun state ->
        p_space state
        ; let msg = p_atom state in
          p_crlf (fun state -> `Ok (msg, state)) state)
       / (p_crlf @ p (`List None))
       @ fun msg -> p (`List (Some msg)))
      state
  | "RETR" ->
    p_space state;
    let msg = p_atom state in
    p_crlf (p (`Retr msg)) state
  | "DELE" ->
    p_space state;
    let msg = p_atom state in
    p_crlf (p (`Dele msg)) state
  | "NOOP" -> p_crlf (p `Noop) state
  | "RSET" -> p_crlf (p `Rset) state
  | c      -> p_crlf (p (`Unknow c)) state

let p_to_crlf p state =
  let rec aux has_cr state =
    match cur_chr state with
    | '\n' when has_cr ->
      junk_chr state; p state
    | '\r' ->
      junk_chr state; aux true state
    | chr ->
      junk_chr state; aux false state
  in

  aux false state

let p_response p state =
  Printf.printf "state: try to read a response\n%!";

  match cur_chr state with
  | '+' -> junk_chr state; p_str "OK" state; p_to_crlf (p `Ok) state
  | '-' -> junk_chr state; p_str "ERR" state; p_to_crlf (p `Err) state
  | chr -> raise (Error.Error (Error.err_unexpected chr state))

let p_dot p state = p_chr '.' state; p state

let p_body p state =
  Printf.printf "state: try to read body\n%!";

  let stop =
    (p_crlf @ p_dot @ p_crlf (fun state -> `Ok ((), state)))
    / (fun state -> `Continue state)
    @ (fun () state -> `Stop state)
  in

  let buf = Buffer.create 16 in

  let rec aux state =
    let rec catch = function
      | `Stop state -> p (Buffer.contents buf) state
      | #Error.err as err -> err
      | `Read (buf, off, len, k) ->
        Printf.printf "> need to read\n%!";
        `Read (buf, off, len, (fun i -> catch @@ safe k i))
      | `Continue state ->
        match peek_chr state with
        | Some chr ->
          Buffer.add_char buf (cur_chr state)
          ; junk_chr state
          ; aux state
        | None -> read_line aux state
    in

    catch (stop state)
  in

  aux state

let p_err _ = `Err
let p_ok _  = `Ok

let expect_body = function
  | `List None
  | `Retr _    -> true
  | _          -> false

let decode state x =
  let body = expect_body x in
  (read_line
   @ p_response
   @ function
     | `Err -> p_err
     | `Ok when body -> p_body @ fun o _ -> `Body o
     | `Ok -> p_ok) state

let w_crlf k state =
  w "\r\n" k state

let w_command = function
  | `Quit            -> w "QUIT" $ w_crlf
  | `Stat            -> w "STAT" $ w_crlf
  | `List (Some msg) -> w "LIST" & w msg $ w_crlf
  | `List None       -> w "LIST" $ w_crlf
  | `Retr msg        -> w "RETR" & w msg $ w_crlf
  | `Dele msg        -> w "DELE" & w msg $ w_crlf
  | `Noop            -> w "NOOP" $ w_crlf
  | `Rset            -> w "RSET" $ w_crlf
  | `User user       -> w "USER" & w user $ w_crlf
  | `Pass pass       -> w "PASS" & w pass $ w_crlf

let w_ret _ = `Ok

let encode state = function
  | `Command cmd -> w_command cmd (flush w_ret) state
