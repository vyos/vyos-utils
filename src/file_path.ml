type opts = {
  must_be_file : bool;
  parent : string option;
  strict : bool; 
}

let default_opts = {
  must_be_file = true;
  parent = None;
  strict = false
}

let opts = ref default_opts

let path_arg = ref ""

let args = [
    ("--file", Arg.Unit (fun () -> opts := {!opts with must_be_file=true}), "Path must point to a file and not a directory (default)");
    ("--directory", Arg.Unit (fun () -> opts := {!opts with must_be_file=false}), "Path must point to a directory");
    ("--parent-dir", Arg.String (fun s -> opts := {!opts with parent=(Some s)}), "Path must be inside specific parent directory");
    ("--strict", Arg.Unit (fun () -> opts := {!opts with strict=true}), "Treat warnings as errors");
]
let usage = Printf.sprintf "Usage: %s [OPTIONS] <path>" Sys.argv.(0)

let () = if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
let () = Arg.parse args (fun s -> path_arg := s) usage

let fail msg =
  let () = print_endline msg in
  exit 1

let () =
  let path = !path_arg in
  let opts = !opts in
  (* First, check if the file/dir path exists at all. *)
  let exists = FileUtil.test FileUtil.Exists path in
  if not exists then Printf.ksprintf fail {|Incorrect path %s: no such file or directory|} path else
  (* If yes, check if it's of the correct type: file or directory. *)
  let is_file =	FileUtil.test FileUtil.Is_file path in
  if ((not is_file) && opts.must_be_file) then Printf.ksprintf fail {|%s is a directory, not a file|} path else 
  if (is_file && (not opts.must_be_file)) then Printf.ksprintf fail {|%s is a file, not a directory|} path else
  match opts.parent with
  | None ->
    exit 0
  | Some parent ->
    if not (FilePath.is_subdir (FilePath.reduce path) (FilePath.reduce parent)) then
    let msg = Printf.sprintf {|Path %s is not under %s directory|} path parent in
    if opts.strict then fail msg else Printf.printf "Warning: %s\n" msg
