# This file pins a version of Nixpkgs which is used only for the repository's
# Haskell toolchain (and is thus independent of general tooling which may be
# pulled in by `direnv`). Specifically, it is geared towards providing a
# compiler (GHC) and set of base libraries that are capable of building
# fully-statically-linked binaries.

let
  # The pattern here is that we will pull a "vanilla" Nixpkgs pin and use an
  # overlay to add static-specific derivations (such as a GHC capable of
  # building fully-staticallly-linked binaries). Why do we even bother with a
  # "vanilla" Nixpkgs? Well, there are some tools in the Haskell toolchain that
  # don't have to be statically built (e.g. build-time tools that retrieve or
  # generate code, like `stack`, or `happy`). By pulling these from a "vanilla"
  # Nixpkgs pin, we should get packages which a. work more often than not and b.
  # are more frequently cached by standard Nixpkgs infrastructure.
  baseNixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-06-22";

    # Update the "name" attribute if/when you change this
    url = https://github.com/NixOS/nixpkgs/archive/dca182df882db483cea5bb0115fea82304157ba1.tar.gz;

    # You can obtain an appropriate hash using
    # `nix-prefetch-url --unpack <url>`.
    sha256 = "0193bpsg1ssr93ihndyv7shz6ivsm8cvaxxl72mc7vfb8d1bwx55";
  };

  # `static-haskell-nix` is a repository maintained by @nh2 that documents and
  # implements the litany of tricks necessary to construct a Nix-Haskell
  # toolchain capable of building fully-statically-linked binaries. We are
  # generally only after the GHC derivation from that repository (since Bazel
  # will be building the rest), but we need it nonetheless (thanks, @nh2!). So
  # we pin a version here that exposes the GHC version we are interested in.
  staticHaskellNixpkgs = builtins.fetchTarball
    https://github.com/nh2/static-haskell-nix/archive/dbce18f4808d27f6a51ce31585078b49c86bd2b5.tar.gz;

  # The `static-haskell-nix` repository contains several entry points for e.g.
  # setting up a project in which Nix is used solely as the build/package
  # management tool. We are only interested in the set of packages that underpin
  # these entry points, which are exposed in the `survey` directory's
  # `approachPkgs` property.
  staticHaskellPkgs = (
    import (staticHaskellNixpkgs + "/survey/default.nix") {}
  ).approachPkgs;

  # With all the pins and imports done, it's time to build our overlay, in which
  # we are interested in:
  #
  # * Providing a GHC that is capable of building fully-statically-linked
  #   binaries.
  #
  # * Providing any system-level/C libraries and packages that we'll need that
  #   aren't already in the `static-haskell-nix` package set (see below for
  #   examples). These are the packages/libraries we'll link when building
  #   Haskell code that has e.g. C dependencies (think PostgreSQL, SSH2, etc.).
  #
  # We accomplish this by exposing the `static-haskell-nix` package set
  # wholesale as `staticHaskell` and extending _that_ with the things we need.
  # If you don't like this approach/would like to structure your Nix derivations
  # differently, feel free -- the only important bits are the leaf packages
  # (e.g. GHC, etc.) themselves.
  overlay = self: super: {
    staticHaskell = staticHaskellPkgs.extend (selfSH: superSH: {
      # We start with GHC, the compiler and arguably the most important bit. The
      # GHC in `static-haskell-nix` is a good base (it uses Musl via
      # `pkgsMusl` and so has a C library layer designed for static linking) but
      # is not quite perfect. Specifically, we make the following tweaks:
      #
      # * (pic-dynamic) At present, upstream Nixpkgs passes `-fPIC` when
      #   `enableRelocatableStaticLibs` is activated but this is actually not
      #   sufficient for GHC to generate 100% relocatable static library code.
      #   We must also pass `-fexternal-dynamic-refs`, lest GHC generate
      #   `R_X86_64_PC32` relocations which break things. We pass these flags
      #   both when building core GHC libraries (e.g. `base`, `containers`;
      #   through `GhcLibHcOpts`) and when building the GHC runtime system (RTS;
      #   through `GhcRtsHcOpts`). See the `preConfigure` phase for more
      #   information.
      ghc = (superSH.ghc.override {
        # See (pic-dynamic)
        enableRelocatedStaticLibs = true;
        enableShared = false;
      }).overrideAttrs (oldAttrs: {
        preConfigure = ''
          # See (pic-dynamic)
          ${oldAttrs.preConfigure or ""}
          echo "GhcLibHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
          echo "GhcRtsHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
        '';
      });

      # A custom derivation that contains both static and dynamic libraries.
      # Note that setting `static = false` isn't sufficient as the derivation at
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/libraries/openssl/default.nix
      # will remove `.a` files in this case. Consequently we build with
      # `static = false` and then explicitly copy the static libraries _back in_
      # from the existing derivation (which sets `static = true`).
      #
      openssl_both =
        (superSH.openssl.overrideAttrs (old: {
          postInstall = ''
            ${old.postInstall}

            cp ${superSH.openssl.out}/lib/*.a $out/lib
          '';
        })).override {
          static = false;
        };

    });
  };

in
  args@{ overlays ? [], ... }:
    import baseNixpkgs (args // {
      overlays = [overlay] ++ overlays;
    })
