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
          
          wrapper = pkgs.runCommand "xmonad-wrapper" {
            buildInputs = [ pkgs.makeWrapper ];
          } ''
            mkdir -p $out/bin;
            cp ${packages.xmonad}/bin/xmonad $out/bin/xmonad-${pkgs.stdenv.hostPlatform.system};
            wrapProgram $out/bin/xmonad-${pkgs.stdenv.hostPlatform.system} \
                         --set XMONAD_XMOBAR "${packages.xmobar}/bin/xmobar" \
                         --set XMONAD_XMESSAGE "${packages.xmessage}/bin/xmessage";
            '';
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
              xsession.windowManager.command = "${packages.wrapper}/bin/xmonad-${pkgs.stdenv.hostPlatform.system}";
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
