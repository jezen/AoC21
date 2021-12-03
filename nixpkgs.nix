let
    # https://github.com/NixOS/nixpkgs/tree/nixos-20.09
    rev = "42809feaa9f7474f5566a5c6e8e317e15e39160e";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
 in import (builtins.fetchTarball url)
