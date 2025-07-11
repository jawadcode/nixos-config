{
  description = "Ixnay Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # wrapper-manager = {
    #   url = "github:ViperML/wrapper-manager";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs = inputs @ {
    nixpkgs,
    home-manager,
    # wrapper-manager,
    ...
  }: {
    nixosConfigurations = {
      ixnay = let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          config = {
            permittedInsecurePackages = ["ventoy-1.1.05"];
            allowUnfree = true;
          };
        };
      in
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          inherit pkgs;
          modules = [
            {
              nix.settings = {
                substituters = [
                  "https://cache.iog.io"
                ];
                trusted-public-keys = [
                  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
                ];
                experimental-features = ["nix-command" "flakes"];
                auto-optimise-store = true;
              };
            }
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.qak = import ./home.nix;
            }
            ./configuration.nix
          ];
          specialArgs = {inherit inputs;};
        };
    };
  };
}
