{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    # nix-doom-emacs-unstraightened = {
    #   url = "github:marienz/nix-doom-emacs-unstraightened";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    # doom-config = {
    #   url = "git+https://github.com/jawadcode/doom-config.git?submodules=1";
    #   flake = false;
    # };
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }: {
    nixosConfigurations.hp-sauce = let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
      nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {inherit inputs;};
        modules = [
          {
            # nixpkgs.overlays = [inputs.nix-doom-emacs-unstraightened.overlays.default];
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
