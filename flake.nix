{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";
    gram-extensions.url = "git+https://codeberg.org/niklaskorz/nix-gram-extensions.git";
  };
  outputs = { nixpkgs, nixpkgs-unstable, emacs-overlay, gram-extensions, ... }: {
    nixosConfigurations.hp-sauce =
      let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import emacs-overlay) ];
          config.allowUnfree = true;
        };
      in
      nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {
          nixpkgs-unstable = import nixpkgs-unstable {
            inherit system;
            config.allowUnfree = true;
          };
          wasip2Pkgs = gram-extensions.inputs.nixpkgs.legacyPackages.${system}.pkgsCross.wasm32-wasip2;
          gram-extensions = gram-extensions.packages.${system};
        };
        modules = [
          {
            nix.settings = {
              experimental-features = [
                "nix-command"
                "flakes"
              ];
              substituters = [ "https://nix-community.cachix.org" ];
              trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
              auto-optimise-store = true;
            };
          }
          ./configuration.nix
        ];
      };
  };
}
