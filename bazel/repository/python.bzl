load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_python_configure")
load("@rules_python//python:repositories.bzl", "py_repositories")

def setup_python():
    py_repositories()

    # At present, Python isn't a language that is built by this repository, only
    # one needed for toolchain support (e.g. scripts that run as part of
    # `rules_haskell`). Consequently it is considered "tooling", not a language
    # and so we take it from the tooling Nixpkgs pin. If you are considering
    # building Python targets in your repository, you may want to consider
    # having a dedicated Python Nixpkgs pin or other alternative approaches.
    nixpkgs_python_configure(repository = "@nixpkgs_tooling")
