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

$ ldd bazel-bin/example-service/impl
$       not a dynamic executable
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
```

* Loading a REPL for a Haskell binary

```bash
$ bazel run //example-service:impl@repl
```

* Loading a REPL for a Haskell binary and a library it depends on

```bash
$ bazel run ///example-service:impl-ifc-repl
```
