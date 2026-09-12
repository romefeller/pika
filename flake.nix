{
  description = "Pika Cabal development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          compiler = pkgs.haskellPackages.ghcWithPackages (p: [
            p.mtl p.parsec p.stm p.transformers
          ]);
        in {
          default = pkgs.mkShell {
            packages = [ compiler pkgs.cabal-install pkgs.z3 ];
          };
        });
    };
}
