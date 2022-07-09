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
          pstree = pkgs.pstree;
          
          wrapper = pkgs.writeShellApplication {
            name = "xmonad-wrapper";

            runtimeInputs = [ packages.xmonad
                              packages.xmobar
                              packages.xmessage
                              packages.pstree
                            ];
            text = "exec xmonad";
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
          buildInputs =
            [
              (pkgs.haskellPackages.ghcWithPackages (p: [
                p.cabal-install
                p.ghcid
                p.hlint
                p.ormolu
                p.haskell-language-server
              ]))
            ];
        };
      });
}
