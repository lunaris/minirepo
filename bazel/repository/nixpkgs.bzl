load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository")

def setup_nixpkgs():
    # Defines a Nixpkgs repository that uses the tooling pin defined in
    # `nix/tooling`.
    nixpkgs_local_repository(
        name = "nixpkgs_tooling",
        nix_file = "//nix/tooling:default.nix",
    )
