{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };
  outputs = _inputs @ {
    self,
    nixpkgs,
    ...
  }: {
    nixosConfigurations.allbuch-nix = let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
      nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        modules = [
          {
            # nixpkgs.overlays = [
            #   (import (
            #     builtins.fetchTarball {
            #       url = "https://github.com/nix-community/emacs-overlay/archive/27ced263ed6b7a6968f9f449d66aa299cb0f14a7.zip"; # ugh
            #       sha256 = "sha256:0v6pl0zhs476hdfxdhaqk8y5nvibk4nra6rqxmfrq8a7fh230vv2";
            #     }
            #   ))
            # ];
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
