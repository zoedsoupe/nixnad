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
      dracula-vim
      rainbow
      vim-surround
      direnv-vim
      vim-commentary
      delimit-mate
      orgmode
      bullets-vim
      telescope-nvim
      nvim-colorizer-lua
      dashboard-nvim
      gitsigns-nvim
      vim-haskell-module-name
    ] ++ map pluginWithConfig [
      vim-polyglot
      (pluginWithDeps nvim-tree-lua [ nvim-web-devicons ])
      (pluginWithDeps galaxyline-nvim [ nvim-web-devicons ])
      (pluginWithDeps nvim-bufferline-lua [ nvim-web-devicons ])
    ] ++ extraPlugins;
  };
}
