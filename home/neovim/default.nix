{ pkgs, lib, ... }:

let
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-neovim-plugins.nix {};

  pluginWithDeps = plugin: deps: plugin.overrideAttrs (_: { dependencies = deps; });

  pluginWithConfig = plugin: {
    plugin = plugin;
    config = "lua require('matthew.${plugin.pname}')";
  };

  extraPlugins = with plugins; [
    popup-nvim
    plenary-nvim
  ];
in 
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  xdg.configFile."nvim/lua".source = ./lua;
  xdg.configFile."nvim/colors".source = ./colors;

  programs.neovim = {
    package = pkgs.neovim-nightly;
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = "lua require('init')\n";
    plugins = with plugins; 
    [
      vim-rescript
      indentLine
      editorconfig-vim
      agda-vim
      emmet-vim
      rainbow
      vim-surround
      vimtex
      direnv-vim
      vim-commentary
      ultisnips
      vim-snippets
      neoclip
      orgmode
      bullets-vim
      telescope-nvim
      vim-highlightedyank
      nvim-colorizer-lua
      dashboard-nvim
      nvim-autopairs
      vim-haskell-module-name
    ] ++ map pluginWithConfig [
      vim-polyglot
      gitsigns-nvim
      (pluginWithDeps nvim-tree-lua [ nvim-web-devicons ])
      (pluginWithDeps galaxyline-nvim [ nvim-web-devicons ])
    ] ++ extraPlugins;
  };
}
