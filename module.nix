{ config, lib, pkgs, ... }:
with lib;
{
  options = {
    dotfiles.enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.dotfiles.enable {
    xsession.windowManager.xmonad.config = ./xmonad/xmonad.hs;
    home.file.".xmobarrc".source = ./xmonad/xmobarrc.hs;
  };
}
        
