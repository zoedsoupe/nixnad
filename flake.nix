{
  description = "nixnad";

  inputs =
    {
      home-manager = {
        url = "github:nix-community/home-manager/release-21.05";
        inputs.nixpkgs.follows = "nixpkgs";
      };
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
      nixpkgs-latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };

  outputs = inputs@{ self, nixpkgs, nixpkgs-latest, home-manager, ... }:
  with import ./global-config.nix;

  let
  system = "x86_64-linux";
  environment-shell = ''
    function nix-repl {
      nix repl "${rootPath}/repl.nix" "$@"
    }
    export NIXPKGS_ALLOW_UNFREE=1
    export NIX_PATH=nixpkgs=${nixpkgs}:nixpkgs-overlays=${builtins.toString rootPath}/overlay.nix:nixpkgs-latest=${nixpkgs-latest}:home-manager=${home-manager}:nixos-config=${(builtins.toString rootPath) + "/nodes/$HOSTNAME/default.nix"}
  '';

  hm-config = home-manager.lib.home-manager-configuration;
  nixos-config = { mainModule }: nixpkgs.lib.nixos-system {
    inherit pkgs system;

    modules = [ (main-module) rev-module ];
  };
  overlays = [
    (import ./overlay.nix)
    (import "${home-manager}/overlay.nix")
  ];
  pkgs = import nixpkgs {
    inherit overlays system;

    config = {
      allowUnfree = true;
    };
  };
  rev-module = ({ pkgs, ... }: {
    system.configuration-revision =
      if (self ? rev) then
        builtins.trace "detected flake hash: ${self.rev}" self.ref
      else
        builtins.trace "flake hash not detected!" null;
  });
  in {
  inherit pkgs overlays environment-shell;

  home-configurations = {
    main = hm-config {
      inherit system username pkgs;

      configuration = import ./home;
      homeDirectory = "/home/${username}";
    };
  };
  nixos-configurations = {
    acer-nix = nixos-config {
      mainModule = ./nodes/acer-nix;
    };
    bootstrap = nixos-config {
      mainModule = ./nodes/bootstrap;
    };
  };

  dev-shell.x86_64-linux = pkgs.mkShell {
    name = "nixnad-shell";
    buildInputs = [];
    shellHook = ''
      ${environment-shell}
      echo '${environment-shell}'
      echo Shell setup complete!
    '';
  };
};
}
