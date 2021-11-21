{
  description = "nixnad";

  inputs =
    {
      home-manager = {
        url = "github:nix-community/home-manager/release-21.05";
        inputs.nixpkgs.follows = "nixpkgs";
      };
      unstable.url = "github:nixos/nixpkgs/nixos-unstable";
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
      nixpkgs-master.url = "github:NixOS/nixpkgs/master";
      nixpkgs-latest.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      neomat.url = "github:zoedsoupe/neomat";
      emacs = {
        url = "github:nix-community/emacs-overlay";
        inputs.nixpkgs.follows = "master";
      };
      emacsmat = {
        url = "github:zoedsoupe/emacsmat";
        flake = false;
      };
    };

  outputs = { self, ... }@inputs:
  let
    inherit (inputs)
      nixpkgs
      nixpkgs-latest
      home-manager
      unstable
      neomat
    ;
    inherit (pkgs.lib) nixosSystem;
    inherit (builtins) toString trace;
    inherit (home-manager.lib) homeManagerConfiguration;

    pkgs = import nixpkgs {
      inherit overlays;
      inherit (global) system;
      config = {
        allowUnfree = true;
      };
    };

    epkgs = import nixpkgs {
      inherit (global) system;
      overlays = [ self.overlays.emacs ]; 
    };

    global = rec {
      username = "zoedsoupe";
      email = "zoey.spessanha@zeetech.io";
      selected-desktop-environment = "gnome";
      rootPath = builtins.toString ./.;
      rooPathNix = rootPath;
      environmentShell = ''
          export NIXPKGS_ALLOW_UNFREE=1
          export NIXNAD_ROOT_PATH=/home/$USER/documents/privy/nixnad
          export NIX_PATH=nixpkgs=${nixpkgs}:nixpkgs-overlays=$NIXNAD_ROOT_PATH/compat/overlay.nix:nixpkgsLatest=${nixpkgs-latest}:home-manager=${home-manager}:nixos-config=$NIXNAD_ROOT_PATH/nodes/$HOSTNAME/default.nix
        '';
      system = "x86_64-linux";
    };

    extra-args = {
      inherit self;
      inherit global;
      cfg = throw "your past self made a trap for non compliant code after a migration you did, now follow the stacktrace and go fix it";
    };

    overlays = [
      (import ./overlay.nix self)
      (import "${home-manager}/overlay.nix")
    ];

    hm-config = config:
      let
        source = config // {
          extraSpecialArgs = extra-args;
          inherit pkgs;
        };
        evaluated = homeManagerConfiguration source;
      in evaluated // { inherit source; };

    nixos-config = {main-module, extra-modules ? []}:
    let
      rev-module = { pkgs, ... }: {
        system.configurationRevision = if (self ? rev) then
          trace "detected flake hash: ${self.rev}" self.rev
        else
          trace "flake hash not detected!" null;
      };
      source = {
        inherit pkgs;
        inherit (global) system;
        modules = [ rev-module (main-module) ] ++ extra-modules;
        specialArgs = extra-args;
      };
      eval = import "${nixpkgs}/nixos/lib/eval-config.nix";
      override = my-source: fn: let
        source-processed = my-source // (fn my-source);
        evaluated = eval source-processed;
      in evaluated // {
        source = source-processed;
        override = override source-processed;
      };
    in override source (v: {});
  in {
    inherit extra-args;
    inherit (global) environment-shell;

    packages."${global.system}" = {
      emacs = epkgs.emacsUnstable;
    };

    homeConfigurations = {
      main = hm-config {
        configuration = import ./home/default.nix;
        homeDirectory = "/home/${global.username}";
        inherit (global) system username;
      };
    };

    nixosConfigurations = {
      acer-nix = nixos-config {
        main-module = ./nodes/acer-nix;
      };
      bootstrap = nixos-config {
        main-module = ./nodes/bootstrap;
      };
    };

    dev-shell.x86_64-linux = pkgs.mkShell {
      name = "nixnad-shell";
      buildInputs = [];
      shellHook = ''
        ${global.environment-shell}
        echo '${global.environment-shell}'
        echo Shell setup complete!
      '';
    };
  };
}
