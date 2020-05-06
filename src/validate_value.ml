type check = Regex of string | Exec of string

let validate_value value_constraint value =
    match value_constraint with
    | Regex s ->
        (try
           let _ = Pcre.exec ~pat:s value in true
       with Not_found -> false)
    | Exec c ->
        (* XXX: Using Unix.system is a bad idea on multiple levels,
                especially when the input comes directly from the user...
                We should do something about it.
         *)
        let result = Unix.system (Printf.sprintf "%s %s" c value) in
        match result with
        | Unix.WEXITED 0 -> true
        | Unix.WEXITED 127 ->
          let () = Printf.printf "Could not execute validator %s" c in
          false
        | _ -> false

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
  let value = !value in
  let checks = !checks in
  match checks with
  | [] -> exit 0
  | _ ->
    List.iter (fun c -> if (validate_value c value) then exit 0 else ()) checks;
    exit 1
