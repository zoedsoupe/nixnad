{ config, lib, pkgs, ... }:

{
  home-manager.users.matthew = {
    programs.tmux = {
      enable = true;
      disableConfirmationPrompt = true;
      keyMode = "emacs";
      extraConfig = ''
      # Automatically set window title
      set-window-option -g automatic-rename on
      set-option -g set-titles on

      # Reload tmux config
      bind r source-file ~/.tmux.conf

      # THEME
      set -g status-bg black
      set -g status-fg white
      set -g window-status-current-bg white
      set -g window-status-current-fg black
      set -g window-status-current-attr bold
      set -g status-interval 60
      set -g status-left-length 30
      set -g status-left '#[fg=green](#S) #(whoami)'
      set -g status-right '#[fg=yellow]#(cut -d " " -f 1-3 /proc/loadavg)#[default] #[fg=white]%H:%M#[default]'
      '';
    };
  };
}
