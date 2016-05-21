#require "jackson";;
#require "lwt"
#require "tls.lwt";;
#require "ppx_deriving.show";;

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
let eprint_sexp sexp =
  output_string stderr Sexplib.Sexp.(to_string_hum sexp);
  output_string stderr "\n\n";
  flush stderr

let run ic oc state command =
  process ic oc state (Pop.run state command)
  >>= function `Ok ->
               Printf.printf "> +OK\n%!";
               Lwt.return (ic, oc, state)
             | `Err ->
               Printf.printf "> -ERR\n%!";
               Lwt.return (ic, oc, state)
             | `Body s ->
               Printf.printf "> Data:\n%s\n%!" s;
               Lwt.return (ic, oc, state)
             | `List l ->
               Printf.printf "> List:\n%s\n%!" ([%derive.show: (int * int) list] l);
               Lwt.return (ic, oc, state)
             | _ -> Lwt.fail (Failure "Unexpected return")

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
  process ic oc state value >>= function `Ok -> Lwt.return (ic, oc, state) | _ -> Lwt.fail (Failure "Unexpected return")
