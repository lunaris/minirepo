workspace(name = "monorepo")

# The Bazel workspace
#
# This file sets up the dependencies required for Bazel builds in this
# repository. It loads:
#
# * A set of "workspace dependencies", which are the versions of the Bazel rules
#   required to build software (e.g. `rules_haskell`, which builds Haskell
#   binaries and libraries; `rules_nixpkgs`, which integrates Bazel with
#   Nixpkgs). These are all pinned so `dependencies.bzl` so that we can ensure
#   consistent build behaviour.
#
# * Using the aforementioned rules, it configures toolchains for each of the
#   languages used in this repository and any other languages or toolchains that
#   they may depend on. For instance, this repository supports Haskell and so
#   `repository/haskell.bzl` sets up a Haskell toolchain. However, the Haskell
#   rules use Nixpkgs to provide reproducible C and POSIX-compatible shell
#   toolchains, so we also have `repository/nixpkgs.bzl` and
#   `repository/posix.bzl` to set those up too. In general, `repository/<X>.bzl`
#   does its best to set up a reproducible build toolchain for <X>.

load("@//bazel/repository:dependencies.bzl", "workspace_dependencies")

workspace_dependencies()

load("@//bazel/repository:go.bzl", "setup_go")
load("@//bazel/repository:haskell.bzl", "setup_haskell")
load("@//bazel/repository:nixpkgs.bzl", "setup_nixpkgs")
load("@//bazel/repository:posix.bzl", "setup_posix")
load("@//bazel/repository:python.bzl", "setup_python")

setup_go()
setup_haskell()
setup_nixpkgs()
setup_posix()
setup_python()

# `rules_docker` has some fiddliness with the order in which its dependencies
# and such must be loaded, so we can't (as far as the author can tell) neatly
# abstract them into `repository/docker.bzl`. That said it's straightforward
# enough to load them directly here.

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories"
)
container_repositories()

load(
    "@io_bazel_rules_docker//cc:image.bzl",
    container_cc_image_respositories = "repositories",
)
container_cc_image_respositories()

load("@//bazel/repository:docker.bzl", "setup_docker")
setup_docker()
