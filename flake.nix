{
  description = "Flake for XMonad dotfiles";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      rec {
        dotfiles = pkgs.symlinkJoin {
          name = "dotfiles";
          paths = [ ./xmonad ./nyxt ];
        };

        overlay = final: prev: { dotfiles = prev.dotfiles; };
        defaultPackage = dotfiles;
      });
}
