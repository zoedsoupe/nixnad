{ pkgs, lib, ... }:

with pkgs.vimUtils;

{
  vim-rescript = buildVimPluginFrom2Nix {
    name = "vim-rescript";
    src = pkgs.fetchFromGitHub {
      owner = "rescript-lang";
      repo = "vim-rescript";
      rev = "HEAD";
      sha256 = "1qzf1g00abj658nvp45nkzjwwdwhbhswpdndrwzsf7y3h2knjlx0";
    };
  };

  delimit-mate = buildVimPluginFrom2Nix {
    name = "delimitMate";
    src = pkgs.fetchFromGitHub {
      owner = "Raimondi";
      repo = "delimitMate";
      rev = "HEAD";
      sha256 = "0vjs11bx5zp6xqny5fd3lhqqvqaz6xjgncyga7hb0x5v6zng7gaj";
    };
  };

  bullets-vim = buildVimPluginFrom2Nix {
    name = "bullets-vim";
    src = pkgs.fetchFromGitHub {
      owner = "dkarter";
      repo = "bullets.vim";
      rev = "HEAD";
      sha256 = "14zbvl0wzbg1a35hya6ii31mamsmmzzwl6lfs4l6vmiz377k06gg";
    };
  };

  orgmode = buildVimPluginFrom2Nix {
    name = "orgmode";
    src = pkgs.fetchFromGitHub {
      owner = "kristijanhusak";
      repo = "orgmode.nvim";
      rev = "HEAD";
      sha256 = "0rfa8cpykarcal8qcfp1dax1kgcbq7bv1ld6r1ia08n9vnqi5vm6";
    };
  };
}
