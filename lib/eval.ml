type t =
  { dec : Decoder.t
  ; enc : Encoder.t
  ; state : [ `Auth | `Trans ] }

let encode x k c =
  let rec loop = function
    | `Partial (s, i, l, k) ->
      `Write (s, i, l, (fun n -> loop (k n)))
    | `Ok -> k c
  in

  loop (Pop.encode c.enc x)

let decode x k c =
  let rec loop = function
    | `Read (s, i, l, k) ->
      `Read (s, i, l, fun n -> loop (k n))
    | (`Ok | `Body _ | `Err) as result -> k result
    | `Error _ -> failwith ("Decoder")
  in

  loop (Pop.decode c.dec x)

let connection () =
  let state =
    { dec = Decoder.make ()
    ; enc = Encoder.make ()
    ; state = `Auth }
  in

  state, decode `Conn (fun x -> x) state

let run state cmd =
  encode (`Command cmd) (decode cmd (fun x -> x)) state
