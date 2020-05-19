let
  staticHaskellNixpkgs = builtins.fetchTarball
    https://github.com/nh2/static-haskell-nix/archive/749707fc90b781c3e653e67917a7d571fe82ae7b.tar.gz;

  staticHaskellPkgs =
    let
      p = import (staticHaskellNixpkgs + "/survey/default.nix") {};
    in
      p.approachPkgs;

in
  args: staticHaskellPkgs
