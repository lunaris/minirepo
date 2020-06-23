let
  baseNixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-06-22";

    # Update the "name" attribute if/when you change this
    url = https://github.com/NixOS/nixpkgs/archive/dca182df882db483cea5bb0115fea82304157ba1.tar.gz;

    # You can obtain an appropriate hash using
    # `nix-prefetch-url --unpack <url>`.
    sha256 = "0193bpsg1ssr93ihndyv7shz6ivsm8cvaxxl72mc7vfb8d1bwx55";
  };

  staticHaskellNixpkgs = builtins.fetchTarball
    https://github.com/nh2/static-haskell-nix/archive/dbce18f4808d27f6a51ce31585078b49c86bd2b5.tar.gz;

  staticHaskellPkgs =
    let
      p = import (staticHaskellNixpkgs + "/survey/default.nix") {};
    in
      p.approachPkgs;

  overlay = self: super: {
    staticHaskell = staticHaskellPkgs.extend (selfSH: superSH: {
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
