import Config

config :dotfiles, :paths,
  dots_path: Path.expand("./dots"),
  config_path: Path.expand("~/.config"),
  home_path: Path.expand("~")
