# This file pulls together the `tooling` Nixpkgs pin and some specific parts of
# the language toolchain pings (e.g. `haskell`) to produce a `nix-shell` that is
# useful for development (e.g. that has `bazel`, `docker`, `psql`, `ghcide`,
# etc.). This can either be used with `nix-shell` explicitly or implicitly
# through `direnv` (for which there is also a configuration file in this
# repository).

{ pkgs ? import ./tooling {} }:

with pkgs;
with darwin.apple_sdk.frameworks;

let
  haskellTooling = import ./haskell {};

  # XXX On Darwin, work around https://github.com/NixOS/nixpkgs/issues/42059.
  # See also https://github.com/NixOS/nixpkgs/pull/41589.
  #
  # This is something that makes it into most of my Haskell/Nix repositories,
  # but is not yet super useful here since static binaries/`pkgMusl` (which e.g.
  # our Haskell toolchain depends on) only works on Linux at present. That said,
  # it is likely possible to set up a Darwin Haskell toolchain that is entirely
  # dynamic for those who want a local development experience on macOS/Darwin.
  cc = stdenv.mkDerivation {
    name = "cc-wrapper-bazel";
    buildInputs = [ stdenv.cc makeWrapper ];
    phases = [ "fixupPhase" ];
    postFixup = ''
      mkdir -p $out/bin
      makeWrapper ${stdenv.cc}/bin/clang $out/bin/clang \
        --add-flags "-isystem ${llvmPackages.libcxx}/include \
                    -F${CoreFoundation}/Library/Frameworks \
                    -F${CoreServices}/Library/Frameworks \
                    -F${Security}/Library/Frameworks \
                    -F${Foundation}/Library/Frameworks \
                    -L${libcxx}/lib \
                    -L${darwin.libobjc}/lib"
    '';
  };

  mkShell = pkgs.mkShell.override {
    stdenv = if stdenv.isDarwin then overrideCC stdenv cc else stdenv;
  };

in
  mkShell {
    buildInputs = [
      # General-purpose tooling
      bazel
      docker
      git
      go
      nix
      postgresql
      python3

      # Language-specific tooling (separate pins)
      haskellTooling.staticHaskell.ghcide
    ];

    # SSL certificates hygiene so that commands like `git` and `nix` work inside
    # a pure shell that would not have access to globally-instead SSL
    # certificates and the like.
    GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
    NIX_SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  } // (if stdenv.isLinux then {
    # Locale-setting hygiene so that pure shells work sensibly.
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  } else {
  })
