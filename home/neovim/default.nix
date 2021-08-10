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
  xdg.configFile."nvim/lua".source = ./lua;
  xdg.configFile."nvim/colors".source = ./colors;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = "lua require('init')";
    plugins = with plugins; 
    [
      vim-rescript
      indentLine
      editorconfig-vim
      emmet-vim
      rainbow
      vim-surround
      vimtex
      direnv-vim
      vim-commentary
      orgmode
      bullets-vim
      telescope-nvim
      vim-highlightedyank
      nvim-colorizer-lua
      dashboard-nvim
      vim-fugitive
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
