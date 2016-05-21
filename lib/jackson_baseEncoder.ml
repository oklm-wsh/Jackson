open Jackson_encoder

let flush p state =
  if state.pos > 0
  then let rec next n =
         if n < state.pos
         then `Partial (state.buffer, n, state.pos - n, fun m -> next (n + m))
         else (state.pos <- 0; p state)
       in next 0
  else p state

let wait k state = `Wait k

let rec writes s k state =
  let len = String.length state.buffer in
  let rec loop j l state =
    let rem = len - state.pos in
    let len = if l > rem then rem else l in
    String.unsafe_blit s j state.buffer state.pos len;
    state.pos <- state.pos + len;
    if len < l then flush (loop (j + len) (l - len)) state else k state
  in
  loop 0 (String.length s) state

let w s k e = writes s k e

let ( & ) x y k e = x (w " " (y k)) e
let ( $ ) x y k e = x (y k) e
