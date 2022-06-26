{
  description = "Flake for XMonad dotfiles";

  inputs = {
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
    
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, flake-utils, xmonad, xmonad-contrib, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          #overlays = [ xmonad.overlay xmonad-contrib.overlay ];
          inherit system;
        }; in
      rec {
        packages = flake-utils.lib.flattenTree {
          xmonad = pkgs.haskellPackages.callCabal2nix "nixmonad" ./xmonad { };
          xmobar = pkgs.haskellPackages.xmobar;
          xmessage = pkgs.xorg.xmessage;

          wrapper = pkgs.writeShellApplication {
            name = "xmonad-wrapper";

            runtimeInputs = [ packages.xmonad packages.xmobar packages.xmessage ];
            text = "xmonad";
          };
          
        };
        
        nixosModule =
          { config, lib, pkgs, ... }:
          with lib; {
            options = {
              dotfiles.enable = mkOption {
                type = types.bool;
                default = false;
              };
            };

            config = mkIf config.dotfiles.enable {
              xsession.windowManager.command = "${packages.wrapper}/bin/xmonad-wrapper";
              home.file.".xmobarrc".source = ./xmonad/xmobarrc.hs;
            };
          };

        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.xmonad p.xmonad-contrib ];
          buildInputs = with pkgs.haskellPackages;
            [
              cabal-install
              ghcid
              hlint
              ormolu
              haskell-language-server
            ];
        };
      });
}
