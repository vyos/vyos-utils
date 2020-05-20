type options = {
  positive: bool;
  nonnegative: bool;
  ranges: string list;
}

let default_opts = {
  positive = false;
  nonnegative = false;
  ranges = []
}

let int_of_string = Big_int.big_int_of_string
let int_of_string_opt = Big_int.big_int_of_string_opt
let big = Big_int.big_int_of_int
let (>=) = Big_int.ge_big_int
let (<=) = Big_int.le_big_int
let (<) = Big_int.lt_big_int
let (>) = Big_int.gt_big_int

let opts = ref default_opts

let number_arg = ref ""

let args = [
    ("--non-negative", Arg.Unit (fun () -> opts := {!opts with nonnegative=true}), "Check if the number is non-negative (>= 0)");
    ("--positive", Arg.Unit (fun () -> opts := {!opts with positive=true}), "Check if the number is positive (> 0)");
    ("--range", Arg.String (fun s -> let optsv = !opts in opts := {optsv with ranges=(s :: optsv.ranges)}), "Check if the number is within a range (inclusive)");
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <number>" Sys.argv.(0)

let () = if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
let () = Arg.parse args (fun s -> number_arg := s) usage

let check_nonnegative opts n =
  if opts.nonnegative && (n < (big 0)) then
  failwith "Number should be non-negative."

let check_positive opts n =
  if opts.positive && (n <= (big 0)) then
  failwith "Number should be positive"

let number_of_string s =
  let n = int_of_string_opt s in
  match n with
  | Some n -> n
  | None ->
     Printf.ksprintf failwith "'%s' is not a valid integer number" s

let range_of_string s =
  let rs = String.split_on_char '-' s |> List.map String.trim |> List.map int_of_string_opt in
  match rs with
  | [Some l; Some r] -> (l, r)
  | _ -> Printf.ksprintf failwith "'%s' is not a valid number range" s

let check_ranges ranges n =
  let in_range (l, r) n = (n >= l) && (n <= r) in
  let res = List.fold_left (fun acc r -> acc || (in_range r n)) false ranges in
  if not res then
  Printf.ksprintf failwith "Number is not in any of allowed ranges"

let () = try
  let opts = !opts in
  let n = number_of_string !number_arg in
  check_nonnegative opts n;
  check_positive opts n;
  if opts.ranges <> [] then
    let ranges = List.map range_of_string opts.ranges in
    check_ranges ranges n
with (Failure err) ->
  print_endline err;
  exit 1

