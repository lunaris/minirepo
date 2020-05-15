let
  baseNixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-02-28-skopeo-patch";

    # Update the "name" attribute if/when you change this
    url = https://github.com/NixOS/nixpkgs/archive/d037d06defd1803c0f0d4afef4e3826a57d3d155.tar.gz;

    # You can obtain an appropriate hash using
    # `nix-prefetch-url --unpack <url>`.
    sha256 = "188ngflznyp9p7x4y0svfgpcyi4lh8fzk5q140pd61plxj3xr3v1";
  };

  overlay = self: super: {
  };

in
  args@{ overlays ? [], ... }:
    import baseNixpkgs (args // {
      overlays = [overlay] ++ overlays;
    })
