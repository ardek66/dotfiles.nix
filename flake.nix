{
  description = "Flake for XMonad dotfiles";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            (haskellPackages.ghcWithPackages (p: [ p.hlint
                                                   p.xmonad p.xmonad-contrib ]))
          ];
        };
      });
}
