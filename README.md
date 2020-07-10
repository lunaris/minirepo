# minirepo

A monorepo template, using Nix, `direnv` and Bazel.

This repository is designed to act as a skeleton for those wishing to use Bazel
to build code in a monorepository. It uses Nix to complement Bazel's hermetic
builds with a hermetic system environment and `direnv` to make this implicitly
available when users `cd` into the repository's directory. At present this
template supports the following:

* A set of Nixpkgs pins designed to provide flexible control over which versions
  of which packages and toolchains are available.

* A Haskell toolchain designed for building _fully-statically-linked binaries_,
  that is, binaries with _no dynamic linking_. Such binaries have minimal
  runtime dependencies and are thus easy to deploy, whether standalone or in
  bare-minimum Docker containers (as supported by e.g. `rules_docker`).

* A Docker toolchain capable of building Docker containers with binaries and
  outputs from this repository (such as the aforementioned static Haskell
  binaries).

## Layout

* `nix` contains Nix code for pinning package sets and tools used to make
  working in this repository reproducible.

* `direnv` contains Bash scripts for having the aforementioned Nix tools be
  added to your `PATH` whenever you are working in this repository. These
  scripts also take care of caching to minimise the performance hits when
  `cd`ing in and out of this repository. `.envrc` in the repository root is the
  file which `direnv` will look for when `cd`ing into this repository and is a
  symlink into the `direnv` directory, which we use to keep things neat.

* `WORKSPACE` defines the Bazel workspace and includes rules for building
  Haskell code, Docker containers, etc. It pulls in definitions from the `bazel`
  directory.

* `example-service` defines an example Haskell service, with a library `ifc`
  (short for "interface") and a binary `impl` (short for "implementation"). Its
  `BUILD.bazel` file defines targets for these two things as well as some other
  test cases (e.g. REPLs, Docker images, Haddock documentation, etc.).

* `hie.yaml` and `.hie-bios` are files that use the aforementioned Bazel rules
  to configure GHCIDE to work with the Haskell projects in this repository.

## Cookbook

### Bazel

#### Haskell

* Building a fully-statically-linked Haskell binary

```bash
$ bazel run //example-service:impl
INFO: Analyzed target //example-service:impl (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl up-to-date:
  bazel-bin/example-service/impl
INFO: Elapsed time: 0.182s, Critical Path: 0.01s
INFO: 0 processes.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
42
Hello, world, from a library!
DefaultCompression
DefaultCompression
No connection string given, no PostgreSQL testing performed

bazel-haskell-playground on  master [$!?]
✦ ➜ ldd bazel-bin/example-service/impl
$       not a dynamic executable
```

* Passing a connection string to test PostgreSQL functionality (and thus more
  complex C library interactions)

```bash
$ PG_CONNECTION_STRING="postgresql://user:pass@localhost" bazel run //example-service:impl
INFO: Analyzed target //example-service:impl (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl up-to-date:
  bazel-bin/example-service/impl
INFO: Elapsed time: 0.158s, Critical Path: 0.01s
INFO: 0 processes.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
42
Hello, world, from a library!
DefaultCompression
DefaultCompression
Testing PostgreSQL
2
```

* Building a Docker container containing a fully-statically-linked Haskell
  binary

```bash
$ bazel run //example-service:impl-image
INFO: Analyzed target //example-service:impl-image (47 packages loaded, 6571 targets configured).
INFO: Found 1 target...
Target //example-service:impl-image up-to-date:
  bazel-bin/example-service/impl-image-layer.tar
INFO: Elapsed time: 9.680s, Critical Path: 0.61s
INFO: 10 processes: 10 linux-sandbox.
INFO: Build completed successfully, 15 total actions
INFO: Build completed successfully, 15 total actions
6b79aa9adb28: Loading layer [==================================================>]  45.74MB/45.74MB
Loaded image ID: sha256:57977ee8dc73dc9d44805c8f209305c76a7d37687d08fd4a8adcd2c4d6b0ac7f
Tagging 57977ee8dc73dc9d44805c8f209305c76a7d37687d08fd4a8adcd2c4d6b0ac7f as bazel/example-service:impl-image
shell-init: error retrieving current directory: getcwd: cannot access parent directories: No such file or directory
42
Hello, world, from a library!
DefaultCompression
DefaultCompression
No connection string given, no PostgreSQL testing performed
```

* Loading a REPL for a Haskell binary

```bash
$ bazel run //example-service:impl@repl
INFO: Analyzed target //example-service:impl@repl (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl@repl up-to-date:
  bazel-bin/example-service/impl@repl@repl
INFO: Elapsed time: 0.116s, Critical Path: 0.01s
INFO: 0 processes.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( example-service/impl/Main.hs, interpreted )
Ok, one module loaded.
Ok, one module loaded.
Loaded GHCi configuration from .../bazel-out/k8-fastbuild/bin/example-service/ghci-repl-script-impl@repl
*Main> main
42
Hello, world, from a library!
DefaultCompression
DefaultCompression
No connection string given, no PostgreSQL testing performed
*Main>
```

* Passing a connection string to test PostgreSQL functionality (and thus more
  complex C library interactions) in a REPL

```bash
$ PG_CONNECTION_STRING="postgresql://user:pass@localhost" bazel run //example-service:impl@repl
INFO: Analyzed target //example-service:impl@repl (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl@repl up-to-date:
  bazel-bin/example-service/impl@repl@repl
INFO: Elapsed time: 0.122s, Critical Path: 0.01s
INFO: 0 processes.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( example-service/impl/Main.hs, interpreted )
Ok, one module loaded.
Ok, one module loaded.
Loaded GHCi configuration from .../bazel-out/k8-fastbuild/bin/example-service/ghci-repl-script-impl@repl
*Main> main
42
Hello, world, from a library!
DefaultCompression
DefaultCompression
Testing PostgreSQL
2
*Main>
```

* Loading a REPL for a Haskell binary and a library it depends on

```bash
$ bazel run //example-service:impl-ifc-repl
INFO: Analyzed target //example-service:impl-ifc-repl (0 packages loaded, 1 target configured).
INFO: Found 1 target...
Target //example-service:impl-ifc-repl up-to-date:
  bazel-bin/example-service/impl-ifc-repl@repl
INFO: Elapsed time: 0.148s, Critical Path: 0.02s
INFO: 0 processes.
INFO: Build completed successfully, 2 total actions
INFO: Build completed successfully, 2 total actions
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Example          ( example-service/ifc/Example.hs, interpreted )
[2 of 2] Compiling Main             ( example-service/impl/Main.hs, interpreted )
Ok, two modules loaded.
Ok, two modules loaded.
Loaded GHCi configuration from .../bazel-out/k8-fastbuild/bin/example-service/ghci-repl-script-impl-ifc-repl
*Example *Main>
```

* Building Haddock documentation for a Haskell library

```bash
$ bazel build //example-service:impl-ifc-docs
INFO: Analyzed target //example-service:impl-ifc-docs (48 packages loaded, 3026 targets configured).
INFO: Found 1 target...
INFO: From HaskellHaddock //example-service:ifc:
Loaded package environment from bazel-out/k8-fastbuild/bin/example-service/compile-package_env-ifc
Loaded package environment from bazel-out/k8-fastbuild/bin/example-service/compile-package_env-ifc
Loaded package environment from bazel-out/k8-fastbuild/bin/example-service/compile-package_env-ifc
Target //example-service:impl-ifc-docs up-to-date:
  bazel-bin/example-service/impl-ifc-docs/base-4.12.0.0
  bazel-bin/example-service/impl-ifc-docs/ghc-prim-0.5.3
  bazel-bin/example-service/impl-ifc-docs/integer-gmp-1.0.2.0
  bazel-bin/example-service/impl-ifc-docs/mtl-2.2.2
  bazel-bin/example-service/impl-ifc-docs/transformers-0.5.6.2
  bazel-bin/example-service/impl-ifc-docs/example-serviceZSifc
  bazel-bin/example-service/impl-ifc-docs/index
INFO: Elapsed time: 1.854s, Critical Path: 0.57s
INFO: 2 processes: 2 linux-sandbox.
INFO: Build completed successfully, 3 total actions
```

* Booting up GHCIDE directly for local development (check out `hie.yaml` and
  `.hie-bios`

```bash
$ ghcide
```

* Booting up an editor or IDE that will start GHCIDE automatically (e.g. VSCode)

```bash
$ code .
```
