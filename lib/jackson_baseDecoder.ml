open Jackson_decoder
module Error = Jackson_error

let safe k state =
  try k state
  with Error.Error (`Error (err, s, i, l)) -> `Error (err, s, i, l)

let read_exact need k state =
  let tmp  = Bytes.create need in
  let have = min need (state.len - state.pos) in
  Bytes.blit state.buffer state.pos tmp 0 have;
  state.pos <- state.pos + have;
  let rec loop rest =
    if rest = 0 then
      safe (k tmp) state
    else
      `Read (tmp, state.pos, need - rest, fun m -> loop (rest - m))
  in
  loop (need - have)

let has_line buff off len =
  let rec loop cr_read j =
    if j >= len then false
    else
      match Bytes.get buff j with
      | '\n' ->
        if cr_read then true else loop false (j + 1)
      | '\r' -> loop true (j + 1)
      | _ -> loop false (j + 1)
  in
  loop false off

let rec roll_back k data state =
  let len = Bytes.length data in
  if state.pos > len
  then begin
    for i = 0 to len - 1
    do Bytes.set state.buffer
         (state.pos - 1 - i)
         (Bytes.get data (len - 1 - i))
    done;
    state.pos <- state.pos - len;

    safe k state
  end else begin
    let new_len    = state.len - state.pos + len in
    let new_buffer = Bytes.create new_len in
    Bytes.blit data 0 new_buffer 0 len;
    Bytes.blit state.buffer state.pos new_buffer len (state.len - state.pos);

    state.pos <- 0;
    state.len <- new_len;
    state.buffer <- new_buffer;

    safe k state
  end

let read_line k state =
  if has_line state.buffer state.pos state.len
  then k state
  else begin
    if state.pos > 0
    then begin
      Bytes.blit state.buffer state.pos state.buffer 0 (state.len - state.pos);
      state.len <- state.len - state.pos;
      state.pos <- 0
    end;

    let rec loop off =
      if has_line state.buffer state.pos off
      then begin
        state.len <- off;
        safe k state
      end else begin
        if off >= Bytes.length state.buffer
        then begin
          let new_buffer = Bytes.create (2 * Bytes.length state.buffer + 1) in
          Bytes.blit state.buffer 0 new_buffer 0 (Bytes.length state.buffer);
          state.buffer <- new_buffer
        end;

        `Read (state.buffer, off,
               Bytes.length state.buffer - off,
               fun n -> loop (off + n))
      end
    in

    loop state.len
  end

let peek_chr state =
  if state.pos < state.len
  then Some (Bytes.get state.buffer state.pos)
  else None

let cur_chr state =
  if state.pos < state.len
  then Bytes.get state.buffer state.pos
  else raise (Error.Error (Error.err_unexpected_eoi state))

let junk_chr state =
  if state.pos < state.len
  then state.pos <- state.pos + 1
  else raise (Error.Error (Error.err_unexpected_eoi state))

let rec p_chr chr p state =
  match peek_chr state with
  | Some c when chr = c ->
    junk_chr state; p state
  | Some _ ->
    raise (Error.Error (Error.err_expected chr state))
  | None ->
    read_line (p_chr chr p) state

let rec p_set l p state =
  match peek_chr state with
  | Some c when List.exists ((=) c) l ->
    junk_chr state; p state
  | Some _ ->
    raise (Error.Error (Error.err_expected_set l state))
  | None ->
    read_line (p_set l p) state

let p_str str p state =
  let rec loop pos state =
    if pos = String.length str
    then p state
    else match peek_chr state with
      | Some chr when String.get str pos = chr ->
        junk_chr state; loop (pos + 1) state
      | Some chr ->
        raise (Error.Error (Error.err_unexpected chr state))
      | None -> read_line (loop pos) state
  in

  loop 0 state

let rec p_while f p state =
  let buf = Buffer.create 16 in

  let rec loop state =
    match peek_chr state with
    | Some chr when f chr ->
      Buffer.add_char buf chr;
      junk_chr state;
      loop state
    | Some _ ->
      p (Buffer.contents buf) state
    | None -> read_line loop state
  in

  match peek_chr state with
  | Some chr when f chr ->
    Buffer.add_char buf chr;
    junk_chr state;
    loop state
  | Some chr -> raise (Error.Error (Error.err_unexpected chr state))
  | None -> read_line (p_while f p) state

let p_try_rule success fail rule state =
  let tmp = Buffer.create 16 in

  Buffer.add_bytes tmp (Bytes.sub state.buffer state.pos (state.len - state.pos));

  let rec loop = function
    | `Error (err, buf, off, len) ->
      state.pos <- 0;
      state.len <- Buffer.length tmp;
      if Buffer.length tmp > Bytes.length state.buffer
      then state.buffer <- Buffer.contents tmp
      else begin
        Bytes.blit_string (Buffer.contents tmp) 0 state.buffer 0 (Buffer.length tmp);
      end;

      fail state
    | `Read (buf, off, len, k) ->
      `Read (buf, off, len,
        (fun writing ->
         Buffer.add_bytes tmp (Bytes.sub buf off writing);
         loop @@ safe k writing))
    | `Ok (data, state) -> safe (success data) state
  in

  loop @@ safe rule state

let ( / ) x y k e = p_try_rule k y x e
let ok data state = `Ok (data, state)
let ( @ ) = ( @@ )
