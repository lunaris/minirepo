let
  baseNixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-06-22";

    # Update the "name" attribute if/when you change this
    url = https://github.com/NixOS/nixpkgs/archive/dca182df882db483cea5bb0115fea82304157ba1.tar.gz;

    # You can obtain an appropriate hash using
    # `nix-prefetch-url --unpack <url>`.
    sha256 = "0193bpsg1ssr93ihndyv7shz6ivsm8cvaxxl72mc7vfb8d1bwx55";
  };

  overlay = self: super: {
  };

in
  args@{ overlays ? [], ... }:
    import baseNixpkgs (args // {
      overlays = [overlay] ++ overlays;
    })
