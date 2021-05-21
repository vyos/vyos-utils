type check = Regex of string | Exec of string | Grp | Faux

let checks = ref []

let find_next_grp n l =
  let rec aux i = function
    | [] -> List.length l
    | h::t ->
      if i > n && h = Grp then i
      else aux (i+1) t
  in aux 0 l

let get_next_range =
  let n = ref (-1) in
  let f () =
    let i = !n and j = (n := find_next_grp !n !checks; !n) in
    (i, j) in
  f

let rec validate_value buf value_constraint value =
  match value_constraint with
  | Faux -> false
  | Grp ->
    let (k, j) = get_next_range () in
    let in_grp i = if i > k && i < j then true else false in
    let conj_checks = List.filteri (fun i _ -> in_grp i) !checks in
    List.fold_left (&&) true (List.map (fun e -> validate_value buf e value) conj_checks)
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

let args = [
  ("--regex", Arg.String (fun s -> checks := (Regex s) :: !checks), "Check the value against a regex");
  ("--exec", Arg.String (fun s -> checks := (Exec s) :: !checks), "Check the value against an external command");
  ("--grp", Arg.Unit (fun () -> checks := (Grp) :: !checks), "Group following arguments, joining results with logical and");
  ("--value", Arg.String (fun s -> value := s), "Value to check");
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <number>" Sys.argv.(0)

let () = Arg.parse args (fun _ -> ()) usage

let buf = Buffer.create 4096

let mask l =
  let (k, j) = get_next_range () in
  let imask i e =
    match e with
    | Grp -> Grp
    | _ -> if i >= k && i <= j then e else Faux in
  List.mapi (fun i e -> imask i e) l

let validate =
  checks := List.rev(!checks);
  let value = !value in
  let disj_checks = mask !checks in
  match disj_checks with
  | [] -> false
  | _ ->
    List.fold_left (||) false (List.map (fun c -> validate_value buf c value) disj_checks)

let _ =
  if validate then exit 0 else
    (* If we got this far, value validation failed.
       Show the user output from the validators.
     *)
    Buffer.contents buf |> print_endline;
    exit 1
