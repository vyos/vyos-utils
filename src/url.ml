(* Extract and validate the scheme part.
   As per the RFC:

  Scheme names consist of a sequence of characters. The lower case
  letters "a"--"z", digits, and the characters plus ("+"), period
  ("."), and hyphen ("-") are allowed. For resiliency, programs
  interpreting URLs should treat upper case letters as equivalent to
  lower case in scheme names (e.g., allow "HTTP" as well as "http").
 *)
let split_scheme url =
  let aux url =
    let res = Pcre.exec ~pat:{|^([a-zA-Z0-9\.\-]+):(.*)$|} url in
    let scheme = Pcre.get_substring res 1 in
    let uri = Pcre.get_substring res 2 in
    (String.lowercase_ascii scheme, uri)
  in
  try Ok (aux url)
  with Not_found -> Error (Printf.sprintf {|"%s" is not a valid URL|} url)

let is_scheme_allowed allowed_schemes scheme =
  match List.find_opt ((=) scheme) allowed_schemes with
  | Some _ -> Ok ()
  | None -> Error (Printf.sprintf {|URL scheme "%s:" is not allowed|} scheme)

let regex_matches regex s =
  try
    let _ = Pcre.exec ~pat:regex s in
    true
  with Not_found -> false

let validate_uri scheme uri =
  match scheme with
  | "http" | "https" ->
    if regex_matches {|//[a-z0-9]+([\-\.]{1}[a-z0-9]+)*(:[0-9]+)*(/.* )?|} uri then Ok ()
    else Error (Printf.sprintf {|"%s" is not a valid URI for the %s URL scheme|} uri scheme)
  | _ -> Ok ()

let validate_url allowed_schemes url =
  let (let*) = Result.bind in
  let* scheme, uri = split_scheme url in
  let* () = is_scheme_allowed allowed_schemes scheme in
  let* () = validate_uri scheme uri in
  Ok ()

let file_transport_schemes = ["http"; "https"; "ftp"; "sftp"; "scp"; "tftp"]

let message_schemes = ["mailto"; "tel"; "sms"]

let allowed_schemes = ref []
let url = ref ""

let args = [
    ("--scheme",
     Arg.String (fun s -> allowed_schemes := s :: !allowed_schemes),
     "Allow only specified schemes");
    ("--file-transport",
     Arg.Unit (fun () -> allowed_schemes := (List.append !allowed_schemes file_transport_schemes)),
     "Allow only file transport protocols (HTTP/S, FTP, SCP/SFTP, TFTP)");
    ("--", Arg.Rest (fun s -> url := s), "Interpret next item as an argument");
]

let usage = Printf.sprintf "Usage: %s [OPTIONS] <URL>" Sys.argv.(0)

let () =
  let () = Arg.parse args (fun s -> url := s) usage in
  (* Force all allowed scheme named to lowercase for ease of comparison. *)
  let allowed_schemes = List.map String.lowercase_ascii !allowed_schemes in
  let res = validate_url allowed_schemes !url in
  match res with
  | Ok () -> ()
  | Error msg ->
    let () = Printf.fprintf stdout "%s" msg in
    exit 1
