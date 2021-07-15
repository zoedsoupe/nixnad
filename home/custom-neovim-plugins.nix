{ pkgs, globalBuildInputs ? [], ... }:

with pkgs.vimUtils;

{
  rainbow = buildVimPluginFrom2Nix {
    name = "rainbow";
    src = pkgs.fetchFromGitHub {
      owner = "luochen1990";
      repo = "rainbow";
      rev = "HEAD";
      sha256 = "168mbdf2h3zhkqrdyyhh0pbkjdvxwida80rdwk8ml97mxxii8ziw";
    };
  };

  surround = buildVimPluginFrom2Nix {
    name = "vim-surround";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-surround";
      rev = "HEAD";
      sha256 = "0aqrqn35xdiy80y7skxfsh3m33n6cdxw6lzz6aspfgzwllx2f0kr";
    };
  };

  commentary = buildVimPluginFrom2Nix {
    name = "vim-commentary";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-commentary";
      rev = "HEAD";
      sha256 = "01lpfcn2hmvxddcf97f4qx5vksxj1hwrxb0c8ri59z9lb9z2hgjd";
    };
  };

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
}
