# minirepo

## Bazel

### Haskell

* Building a fully-statically-linked Haskell binary

```bash
$ bazel run //example-service:impl
INFO: Analyzed target //example-service:impl (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl up-to-date:
  bazel-bin/example-service/impl
INFO: Elapsed time: 0.104s, Critical Path: 0.00s
INFO: 0 processes.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
42
"DefaultCompression"
DefaultCompression
No connection string given, no PostgreSQL testing performed

$ ldd bazel-bin/example-service/impl
$       not a dynamic executable
```

* Passing a connection string to test PostgreSQL functionality (and thus more
  complex C library interactions):

```bash
$ PG_CONNECTION_STRING="postgresql://user:pass@localhost" bazel run //example-service:impl
INFO: Analyzed target //example-service:impl (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl up-to-date:
  bazel-bin/example-service/impl
INFO: Elapsed time: 0.122s, Critical Path: 0.00s
INFO: 0 processes.
INFO: Build completed successfully, 2 total actions
INFO: Build completed successfully, 2 total actions
42
DefaultCompression
DefaultCompression
Testing PostgreSQL
2
```

* Building a Docker container containing a fully-statically-linked Haskell
  binary

```bash
$ bazel run //example-service:impl-image
INFO: Analyzed target //example-service:impl-image (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //example-service:impl-image up-to-date:
  bazel-bin/example-service/impl-image-layer.tar
INFO: Elapsed time: 0.118s, Critical Path: 0.00s
INFO: 0 processes.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
Loaded image ID: sha256:19814ea9e46dc17a7ce1db850f18594898218028649e9b211a8d67ce37343a1e
Tagging 19814ea9e46dc17a7ce1db850f18594898218028649e9b211a8d67ce37343a1e as bazel/example-service:impl-image
42
"DefaultCompression"
DefaultCompression
No connection string given, no PostgreSQL testing performed
```

* Loading a REPL for a Haskell binary

```bash
$ bazel run //example-service:impl@repl
```

* Loading a REPL for a Haskell binary and a library it depends on

```bash
$ bazel run ///example-service:impl-ifc-repl
```

* Loading a REPL for a Haskell binary that uses PostgreSQL (and thus more
  complex C library interaction)

```bash
$ PG_CONNECTION_STRING="postgresql://user:pass@localhost" bazel run //example-service:impl@repl
INFO: Analyzed target //example-service:impl@repl (14 packages loaded, 1178 targets configured).
INFO: Found 1 target...
Target //example-service:impl@repl up-to-date:
  bazel-bin/example-service/impl@repl@repl
INFO: Elapsed time: 13.192s, Critical Path: 0.01s
INFO: 0 processes.
INFO: Build completed successfully, 5 total actions
INFO: Build completed successfully, 5 total actions
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( example-service/impl/Main.hs, interpreted )
Ok, one module loaded.
Ok, one module loaded.
Loaded GHCi configuration from /.../example-service/ghci-repl-script-impl@repl
*Main> main
42
DefaultCompression
DefaultCompression
Testing PostgreSQL
2
```
