# vyos-utils

This repository contains utility binaries used by the VyOS CLI,
written in OCaml for speed and memory safety.

Currently, they are:

* `validate_value` — the top-level validation utility that checks regexes and executes external validators.
* `validators` — validator executables:
  * `file_path` — checks directory and file paths.
  * `numeric` — checks numbers and number ranges.
  * `url` — checks URLs/URIs.
* `completion` — completion helpers:
  * `list_interfaces` — produces lists of network interfaces.
