#require "jackson";;
#require "lwt"
#require "tls.lwt";;

open Lwt.Infix
open Jackson

let rec process ic oc state = function
  | `Read (s, i, l, k) ->
    Lwt_io.read_into ic s i l >>= fun n ->
    process ic oc state (k n)
  | `Write (o, pos, len, k) ->
    Lwt_io.write_from oc o pos len >>= fun n ->
    process ic oc state (k n)
  | `Error e -> Lwt.fail (Failure ("Moon walk"))
  | value -> Lwt.return value

let server_cert = "./certificates/server.pem"
let server_key  = "./certificates/server.key"

type ret = [ `List of (int * int) list | `Body of string | `Ok | `Err ]

let rec process (ic, oc, state) status : ret Lwt.t =
  match status with
  | `Read (s, i, l, k) ->
    Lwt_io.read_into ic s i l >>= fun n ->
    process (ic, oc, state) (k n)
  | `Write (o, p, l, k) ->
    Lwt_io.write_from oc o p l >>= fun n ->
    process (ic, oc, state) (k n)
  | `Error _ as err -> Lwt.fail (Pop.Error.Error err)
  | #ret as value   -> Lwt.return value

let p = Printf.sprintf

let connect ?(port = 995) host =
  X509_lwt.authenticator (`Ca_dir "./certificates") >>= fun authenticator ->
  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun certificate ->
  Tls_lwt.connect_ext
    ~trace:(fun _ -> ())
    Tls.Config.(client ~authenticator ~certificates:(`Single certificate) ())
    (host, port) >>= fun (ic, oc) ->
  let state, value = Pop.connection () in
  process (ic, oc, state) value >>= function
    | `Ok -> Lwt.return (ic, oc, state)
    | _   -> Lwt.fail (Failure (p "Something is wrong with %s:%d" host port))

let p_command () = function
  | `List _ -> "LIST"
  | `Stat   -> "STAT"
  | `Quit   -> "QUIT"
  | `Retr i -> ("RETR " ^ string_of_int i);
  | `Dele _ -> "DELE"
  | `Noop   -> "NOOP"
  | `Rset   -> "RSET"
  | `User _ -> "USER"
  | `Pass _ -> "PASS"

let run command (ic, oc, state) =
  Printf.printf "> run %s\n%!" (p_command () command);

  process (ic, oc, state) (Pop.run state command) >>= function
  | `Err    -> Lwt.fail (Failure (p "Error with command %a" p_command command))
  | `Ok     -> Lwt.return (`Ok, (ic, oc, state))
  | `Body s -> Lwt.return (`Text s, (ic, oc, state))
  | `List l -> Lwt.return (`List l, (ic, oc, state))

let download ?port host username password =
  let ( >?= ) ret next = ret >>= function
    | `Ok, state -> next state
    | _          -> Lwt.fail (Failure (p "Unexpected value"))
  in
  let ( >|= ) ret next = ret >>= function
    | `List l, state -> next l state
    | _          -> Lwt.fail (Failure (p "Unexpected value"))
  in
  let ( >:= ) ret next = ret >>= function
    | `Text s, state -> next s state
    | _              -> Lwt.fail (Failure (p "Unexpected value"))
  in
  connect ?port host
  >>= run (`User username)
  >?= run (`Pass password)
  >?= run (`List None)
  >|= fun l state ->
      Lwt_list.fold_left_s
        (fun (state, acc) (msg, _) ->
         run (`Retr msg) state >:= fun s state -> Lwt.return (state, s :: acc))
        (state, []) l
  >>= fun (state, ret) -> run `Quit state
  >>= fun _ -> Lwt.return ret
