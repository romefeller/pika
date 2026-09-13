{ pkgs ? import (
    let
      locked = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked;
    in builtins.fetchTarball {
      url = "https://github.com/${locked.owner}/${locked.repo}/archive/${locked.rev}.tar.gz";
      sha256 = locked.narHash;
    }
  ) {}
}:

pkgs.mkShell {
  packages = [
    (pkgs.haskellPackages.ghcWithPackages (p: [
      p.mtl
      p.parsec
      p.stm
      p.transformers
    ]))
    pkgs.cabal-install
    pkgs.z3
  ];
}
