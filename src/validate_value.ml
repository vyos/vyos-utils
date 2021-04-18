type check = Regex of string | Exec of string

let validate_value buf value_constraint value =
  match value_constraint with
  | Regex s ->
    (try let _ = Pcre.exec ~pat:s value in true
     with Not_found -> false)
  | Exec c ->
    (* XXX: Unix.open_process_in is "shelling out", which is a bad idea on multiple levels,
       especially when the input comes directly from the user...
       We should do something about it.
     *)
    let chan = Unix.open_process_in (Printf.sprintf "%s \'%s\' 2>&1" c value) in
    let out = try CCIO.read_all chan with _ -> "" in
    let result = Unix.close_process_in chan in
    match result with
    | Unix.WEXITED 0 -> true
    | Unix.WEXITED 127 ->
      let () = Printf.printf "Could not execute validator %s" c in
      false
    | _ ->
      let () = Buffer.add_string buf out; Buffer.add_string buf "\n" in
      false

let value = ref ""

let checks = ref []

let args = [
  ("--regex", Arg.String (fun s -> checks := (Regex s) :: !checks), "Check the value against a regex");
  ("--exec", Arg.String (fun s -> checks := (Exec s) :: !checks), "Check the value against an external command");
  ("--value", Arg.String (fun s -> value := s), "Value to check");
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <number>" Sys.argv.(0)

let () = Arg.parse args (fun _ -> ()) usage

let _ =
  let buf = Buffer.create 4096 in
  let value = !value in
  let checks = !checks in
  match checks with
  | [] -> exit 0
  | _ ->
    List.iter (fun c -> if (validate_value buf c value) then exit 0 else ()) checks;
    (* If we got this far, value validation failed.
       Show the user output from the validators.
     *)
    Buffer.contents buf |> print_endline;
    exit 1
