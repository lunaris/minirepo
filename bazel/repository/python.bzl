load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_python_configure")
load("@rules_python//python:repositories.bzl", "py_repositories")

def setup_python():
    py_repositories()

    nixpkgs_python_configure(repository = "@nixpkgs_tooling")
