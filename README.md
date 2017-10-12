# nix-path

`nix-path` is a tiny tool to manage the build paths used by
[nix](http://nixos.org/nix) in a declarative fashion. It works by setting the
`NIX_PATH` environment variable for you when running nix tools, and gives you
several ways to define the nix path in a controlled way.

Currently, `nix-path` has not been tested enough to be considered anything
other than a proof of concept. If you decide to try it out, please keep that in
mind.

## Installation

```
nix-env -i -f . \
  -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/release-17.09.tar.gz
```

## Quick start

First, let's assume you have a file called `paths.nix` in place, with the
following contents:

```
let

  nixpkgs-master = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";

  nixpkgs-1509 = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-15.09.tar.gz";

in {
  nixpkgs = nixpkgs-1509;

  # Unfortunately, hnix does not yet support 'inherit'
  nixpkgs-master = nixpkgs-master;
  nixpkgs-1509 = nixpkgs-1509;
}
```

Now you can use `nix-path` to inject the above paths into `NIX_PATH`:

```
$ nix-path -f paths.nix nix-build --no-out-link -A vim '<nixpkgs>'
/nix/store/yblqgyrn4jgwfg89qp9041i0n2z26v5b-vim-7.4.827

$ nix-path -f paths.nix nix-build --no-out-link -A vim '<nixpkgs-1509>'
/nix/store/yblqgyrn4jgwfg89qp9041i0n2z26v5b-vim-7.4.827

$ nix-path -f paths.nix nix-build --no-out-link -A vim '<nixpkgs-master>'
/nix/store/abzma4gk26fqwwhfdjxra2x9pcf3g6c1-vim-7.4.827
```

`nix-path` also supports reading paths from the current value of `NIX_PATH`,
and directly from the command line:

```
$ nix-path -e -I nixpkgs=../src/nixpkgs ...
```

The above command first reads `NIX_PATH` (the `-e` option), and then overrides
the `<nixpkgs>` path with `../src/nixpkgs`. That is exactly what `nix-build -I
nixpkgs=../src/nixpkgs` also does.

A more interesting example is to have a set of paths for a project, and then do
local path overrides during development:

```
$ nix-path -f project-paths.nix -f dev-paths.nix ...
```

You can mix any number of `-e`, `-f` and `-I` options as you like. `nix-path`
will combine it all to a single `NIX_PATH` entry, with later arguments
overriding earlier ones.

The `-I` option follows the syntax of the Nix tools, so you can define
multiple, colon-separated path entries in one go or use several `-I` arguments.

As a convenience, completely leaving out any path arguments behaves the same as
running `nix-path -f paths.nix`.

## Path file specification

A path file (specified with the `-f` option) should evaluate to an attribute
set of string-like values (see next section for details on different kinds of
part targets). The name of each attribute will end up as a path prefix in the
nix path. It is not possible to specify non-prefixed (root) paths in a path
file, but you can use the `-I` option to do that.

## Path target specifications

Besides from supporting the ordinary types of nix paths (absolute or relative
local file paths, and tarball URLs), `nix-path` supports git targets. git
targets are defined by any valid git URL (the URL scheme is mandatory), and can
be specified both in path files (`-f`) and directly in command line path
arguments (`-I`).

git targets are not downloaded to the Nix store. Instead, they are fetched to a
cache directory that only the user running `nix-path` has access to. The
`NIX_PATH` is then set up to point to the correct directories within the cache
directory. In the end, the sources might be copied to the Nix store, depending
on how the paths are used within your build.

You can specify a particular revision by appending it to the URL,
separated by a space character (this syntax is used also by Hydra):

```
-I "nixpkgs=ssh://git@github.com/NixOS/nixpkgs.git release-15.09"
```

`nix-path` supports the following git revision specifiers:

  * `HEAD` - refers to the head of the remote
  * `branch name` - refers to the head of the specified branch
  * `refs/xx/yy` - an exact reference
  * `SHA-1 hash` - an exact commit

Sometimes, `nix-path` might interpret an intended git target as an ordinary
nix path instead. That is because nix supports `https://` paths (tarballs) and
`nix-path` can't know if such URLs really are git URLs. To force `nix-path` to
handle a path as a git target, just add a git reference to the URL (you can
for example use `HEAD`).

## Limitations

`nix-path` uses the excellent [hnix](https://github.com/jwiegley/hnix) project.
Unfortunately, `hnix` still lacks support for a lot of Nix language features.
When you hit limits in `hnix`, you will likely see error messages like
`nix-path: Nix/Eval.hs:127:3-76: Non-exhaustive patterns in function go`. If
you keep your path definition files simple enough, the current implementation
should hopefully suffice, but this situation should be improved in the future.
