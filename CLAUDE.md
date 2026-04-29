# CLAUDE.md

## Project purpose
OCaml validator, completion-helper, and operational-mode wrapper binaries that the VyOS CLI invokes at runtime. Validators are referenced from XML interface definitions (`<validator name='numeric' .../>`, `<validator name='url' .../>`, etc.); `vyos-op-run` is the privileged wrapper that looks up and executes operational mode commands on behalf of operator-level users.

## Tech stack
- OCaml; `dune` 2.0 build system; `opam` package metadata in `vyos-utils.opam`.
- Build deps (per opam): `ocamlfind`, `dune >= 2.0`. Trivially small for an OCaml project.
- Debian packaging in `debian/` (`debhelper >= 9`, `quilt`).

## Build / test / run
- Local: `opam install . --deps-only` then `dune build -p vyos-utils`.
- Debian: `dpkg-buildpackage -us -uc -b` produces the `vyos-utils` `.deb`.
- No `dune runtest` suite in tree; validators are exercised in `vyos-1x` smoketests.

## Repository layout
- `src/` — OCaml sources for `validate_value`, validators (`file_path`, `numeric`, `url`), completion helpers (`list_interfaces`), and `vyos_op_run` (operational mode command wrapper).
- `dune-project`, `vyos-utils.opam` — build and package metadata.
- `debian/` — packaging.
- `.github/workflows/` — `check-pr-conflicts.yml`, `pr-mirror-repo-sync.yml`, `trigger-rebuild-repo-package.yml` delegate to `vyos/.github` reusables; `cla-check.yml` delegates to `vyos/vyos-cla-signatures`.

## Cross-repo context
- Listed in `VyOS-Networks/vyos-build-packages/repos.toml` as one of the 14 canonical source packages baked into VyOS images by `vyos/vyos-build`.
- Validator binaries are referenced from XML in `vyos/vyos-1x/interface-definitions/` — that is the runtime consumer.
- Live consumer of the generation-1 mirror pipeline (`pr-mirror-repo-sync.yml@current`) — one of only four repos confirmed live (`vyos-1x`, `vyos-build`, `vyos-utils`, `vyos1x-config`).

## Conventions
- Commit / PR title: `component: T12345: description` (Phorge ID mandatory).
- Default branch `current`. License GPL-2.0 in tree; opam declares `MIT` (note the discrepancy if redistributing).
- Reusable workflows pinned as `uses: vyos/.github/.github/workflows/<X>.yml@current`.

## Mirror relationship
Canonical side. Mirror twin: `VyOS-Networks/vyos-utils` (force-pushed from this repo by the mirror pipeline). Only edit the `vyos/*` side; the VyOS-Networks subpage links back here.

## Notes for future contributors
- Keep dune deps minimal; this binary is on the hot path of every CLI commit.
- New validator? Wire it into `vyos-1x` XML and add a smoketest there.
- LICENSE/opam license mismatch (GPL-2.0 vs MIT) is worth resolving on any non-trivial change.
