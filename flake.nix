{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";
  };
  outputs =
    inputs @ { self
    , nixpkgs
    , emacs-overlay
    , ...
    }: {
      nixConfig = {
        extra-substituters = [ "https://nix-community.cachix.org" ];
        extra-trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
      };
      nixosConfigurations.hp-sauce =
        let
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import self.inputs.emacs-overlay) ];
            config.allowUnfree = true;
          };
        in
        nixpkgs.lib.nixosSystem {
          inherit system pkgs;
          specialArgs = { inherit inputs; };
          modules = [
            {
              nix.settings = {
                experimental-features = [
                  "nix-command"
                  "flakes"
                ];
                auto-optimise-store = true;
              };
            }
            ./configuration.nix
          ];
        };
    };
}
