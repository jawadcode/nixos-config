{
  description = "Ixnay Configuration";

  inputs = {
    nixpkgs.follows = "nixos-cosmic/nixpkgs-stable";
    nixos-cosmic.url = "github:lilyinstarlight/nixos-cosmic";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    nixos-cosmic,
    home-manager,
    ...
  }: {
    nixosConfigurations = {
      ixnay = let
        pkgs = import nixpkgs {
          system = "x86_64-linux";
          config.allowUnfree = true;
        };
      in
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          inherit pkgs;
          modules = [
            {
              nix.settings = {
                substituters = ["https://cosmic.cachix.org/"];
                trusted-public-keys = ["cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="];
              };
            }
            ./configuration.nix
            nixos-cosmic.nixosModules.default
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.qak = import ./home.nix;
            }
          ];
        };
    };
  };
}
