load(
    "@io_bazel_rules_go//go:deps.bzl",
    "go_register_toolchains",
    "go_rules_dependencies",
    "go_wrap_sdk",
)
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

# By default, rules_go will download a Go SDK for your platform and use that.
# The problem is that this sdk is not statically linked and doesn't work on
# NixOS (see https://github.com/bazelbuild/rules_go/issues/1376).
#
# So we fix this by pinning the go toolchain to what nixpkgs provides.

def setup_go():
    go_rules_dependencies()

    nixpkgs_package(
        name = "go",
        repository = "@nixpkgs",
        nix_file_content = _go_for_rules_go,
    )

    go_wrap_sdk(
        name = "go_sdk",
        root_file = "@go//:share/go/GOROOT",
    )

    go_register_toolchains()

# go_wrap_sdk wants to have a label to a file (not directory) at the top of the
# GORROT. Unfortunately, Nix gives us a GOROOT that only contains directories.
# So we use a wrapper that adds a file, called GOROOT at the top of the, well,
# GOROOT.
_go_for_rules_go = """
    with (import <nixpkgs> {});
    let
      go_for_bazel = {stdenv, go}:
        stdenv.mkDerivation rec {
            name = "go_for_bazel";
            version = go.version;

            buildInputs = [ go ];

            phases = [ "installPhase" ];

            installPhase = ''
              runHook preInstall
              GOROOT=$out/share/go
              mkdir -p $GOROOT
              cp -a ${go}/share/go/* $GOROOT
              touch "$GOROOT/GOROOT"
              runHook postInstall
            '';
      };
     in
      pkgs.callPackage go_for_bazel {}
"""
