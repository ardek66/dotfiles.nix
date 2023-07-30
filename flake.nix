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
          xmonadUnwrapped = pkgs.haskellPackages.callCabal2nix "nixmonad" ./xmonad { };
          xmobar = pkgs.haskellPackages.xmobar;
          xmessage = pkgs.xorg.xmessage;
          pstree = pkgs.pstree;
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
              xsession.windowManager.command = "${
                pkgs.writeShellApplication {
                  name = "xmonad-wrapper";
                  runtimeInputs = [ packages.xmonadUnwrapped ];
                  text =
                    ''
                    export XMONAD_XMOBAR="${packages.xmobar}/bin/xmobar";
                    export XMONAD_XMESSAGE="${packages.xmessage}/bin/xmessage";
                    export XMONAD_DATA_DIR="$HOME/.xmonad"
                    export XMONAD_CONFIG_DIR="$HOME/.xmonad"
                    export XMONAD_CACHE_DIR="$HOME/.xmonad"
		    export KDEWM="$HOME/.xmonad/xmonad-${pkgs.stdenv.hostPlatform.system}";
                    $KDEWM &
		    exec /usr/bin/startplasma-x11
                    '';
                }
              }/bin/xmonad-wrapper";
              
              home.file.".xmobarrc".source = ./xmonad/xmobarrc.hs;
              home.file.".xmonad/xmonad-${pkgs.stdenv.hostPlatform.system}" = {
                source = "${packages.xmonadUnwrapped}/bin/xmonad";
                executable = true;
              };
            };
          };

        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.xmonadUnwrapped p.xmonad-contrib ];
          buildInputs =
            [ (pkgs.haskellPackages.ghcWithPackages (p:
              [ p.cabal-install
                p.ghcid
                p.hlint
                p.ormolu
                p.haskell-language-server
              ]))
            ];
        };
      });
}
