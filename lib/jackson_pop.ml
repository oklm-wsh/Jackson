module Error = Jackson_error

module D =
struct
  open Jackson_baseDecoder

  let p_crlf p state =
    p_chr '\r' state
    ; p_chr '\n' state
    ; p state

  let s_crlf p =
    s_chr '\r' @ s_chr '\n' @ p

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
      p_space state
      ; let msg = p_atom state in
        p_crlf (p (`Retr msg)) state
    | "DELE" ->
      p_space state
      ; let msg = p_atom state in
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
    match cur_chr state with
    | '+' -> junk_chr state; p_str "OK" state; p_to_crlf (p `Ok) state
    | '-' -> junk_chr state; p_str "ERR" state; p_to_crlf (p `Err) state
    | chr -> raise (Error.Error (Error.err_unexpected chr state))

  let p_dot p state = p_chr '.' state; p state

  let p_body p state =
    let stop =
      (s_crlf @ p_dot @ s_crlf (fun state -> `Ok ((), state)))
      / (fun state -> `Continue state)
      @ (fun () state -> `Stop state)
    in

    let buf = Buffer.create 16 in

    let rec aux state =
      let rec catch = function
        | `Stop state ->
          p (Buffer.contents buf) state
        | #Error.err as err -> err
        | `Read (buf, off, len, k) ->
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

  let is_digit = function
    | '0' .. '9' -> true
    | _          -> false

  let p_list p =
    let rec loop acc =
      (p_dot @ p_crlf @ fun state -> `Ok ((), state))
      / (fun state ->
         let msg = int_of_string @@ p_while is_digit state in
         p_space state
         ; let size = int_of_string @@ p_while is_digit state in
           p_crlf (read_line @ loop ((msg, size) :: acc)) state)
      @ fun () -> p (List.rev acc)
    in

    read_line (loop [])

  let p_noop p state =
    p state

  let p_err _ = `Err
  let p_ok _  = `Ok

  let extract_data p = function
    | `List None -> p_list @ fun l -> p (`List l)
    | `Retr _    -> p_body @ fun o -> p (`Body o)
    | _          -> p_ok

  let decode state x =
    let p_data = extract_data (fun x _ -> x) x in
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
  { dec : Jackson_decoder.t
  ; enc : Jackson_encoder.t
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
    { dec = Jackson_decoder.make ()
    ; enc = Jackson_encoder.make ()
    ; state = `Auth }
  in

  state, decode `Conn (fun x -> x) state

let run state cmd =
  encode cmd (decode cmd (fun x -> x)) state
