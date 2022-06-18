type options = {
  positive: bool;
  nonnegative: bool;
  allow_float: bool;
  ranges: string list;
  relative: bool;
}

let default_opts = {
  positive = false;
  nonnegative = false;
  allow_float = false;
  ranges = [];
  relative = false
}

let opts = ref default_opts

let number_arg = ref ""

let args = [
    ("--non-negative", Arg.Unit (fun () -> opts := {!opts with nonnegative=true}), "Check if the number is non-negative (>= 0)");
    ("--positive", Arg.Unit (fun () -> opts := {!opts with positive=true}), "Check if the number is positive (> 0)");
    ("--range", Arg.String (fun s -> let optsv = !opts in opts := {optsv with ranges=(s :: optsv.ranges)}), "Check if the number is within a range (inclusive)");
    ("--float", Arg.Unit (fun () -> opts := {!opts with allow_float=true}), "Allow floating-point numbers");
    ("--relative", Arg.Unit (fun () -> opts := {!opts with relative=true}), "Allow relative increment/decrement (+/-N)");
    ("--", Arg.Rest (fun s -> number_arg := s), "Interpret next item as an argument");
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <number>" Sys.argv.(0)

let () = if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
let () = Arg.parse args (fun s -> number_arg := s) usage

let check_nonnegative opts n =
  if opts.nonnegative && (n < 0.0) then
  failwith "Number should be non-negative."

let check_positive opts n =
  if opts.positive && (n <= 0.0) then
  failwith "Number should be positive"

let looks_like_number value =
  try let _ = Pcre.exec ~pat:"^(\\-?)[0-9]+(\\.[0-9]+)?$" value in true
  with Not_found -> false

let is_relative value =
  try let _ = Pcre.exec ~pat:"^[+-][0-9]+$" value in true
  with Not_found -> false

let number_string_from_relative value =
  String.sub value 1 (String.length value - 1)

let get_relative opts s =
  if opts.relative then
    if not (is_relative s) then
      failwith "Value is not a relative increment/decrement"
    else number_string_from_relative s
  else s

let number_of_string opts s =
  if not (looks_like_number s) then Printf.ksprintf failwith "'%s' is not a valid number" s else
  let n = float_of_string_opt s in
  match n with
  | Some n ->
    (* If floats are explicitly allowed, just return the number. *)
    if opts.allow_float then n
    (* If floats are not explicitly allowed, check if the argument has a decimal separator in it.
       If the argument string contains a dot but float_of_string didn't dislike it,
       it's a valid number but not an integer.
     *)
    else if not (String.contains s '.') then n
    (* If float_of_string returned None, the argument string is just garbage rather than a number. *)
    else Printf.ksprintf failwith "'%s' is not a valid integer number" s
  | None ->
     Printf.ksprintf failwith "'%s' is not a valid number" s

let range_of_string opts s =
  let rs = String.split_on_char '-' s |> List.map String.trim |> List.map (number_of_string opts) in
  match rs with
  | [l; r] -> (l, r)
  | exception (Failure msg) ->
    (* Some of the numbers in the range are bad. *)
    Printf.ksprintf failwith "'%s' is not a valid number range: %s" s msg
  | _ ->
    (* The range itself if malformed, like 1-10-20. *)
    Printf.ksprintf failwith "'%s' is not a valid number range" s

let check_ranges ranges n =
  let in_range (l, r) n = (n >= l) && (n <= r) in
  let res = List.fold_left (fun acc r -> acc || (in_range r n)) false ranges in
  if not res then
  Printf.ksprintf failwith "Number is not in any of allowed ranges"

let () = try
  let opts = !opts in
  let s = get_relative opts !number_arg in
  let n = number_of_string opts s in
  check_nonnegative opts n;
  check_positive opts n;
  if opts.ranges <> [] then
    let ranges = List.map (range_of_string opts) opts.ranges in
    check_ranges ranges n
with (Failure err) ->
  print_endline err;
  exit 1

