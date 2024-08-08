{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.opengl = {
    # These two should be enabled by default for sway but I'm not chancing it:
    enable = true;
    driSupport = true;

    # Not strictly necessary but I might want to use WINE at some point
    driSupport32Bit = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    prime = {
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  networking.hostName = "ixnay";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/London";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_GB.UTF-8";
      LC_IDENTIFICATION = "en_GB.UTF-8";
      LC_MEASUREMENT = "en_GB.UTF-8";
      LC_MONETARY = "en_GB.UTF-8";
      LC_NAME = "en_GB.UTF-8";
      LC_NUMERIC = "en_GB.UTF-8";
      LC_PAPER = "en_GB.UTF-8";
      LC_TELEPHONE = "en_GB.UTF-8";
      LC_TIME = "en_GB.UTF-8";
    };
  };

  services.displayManager = {
    sddm = {
      enable = true;
      wayland.enable = true;
    };
    # <rant>
    # the sway module doesn't add the session if you have package == null
    # which is required if you want to configure with home-manager but also have it be available to a display manager
    # but that just defeats the whole point of using configuration.nix to install sway.
    # </rant>
    sessionPackages = [ pkgs.sway ];
    defaultSession = "sway";
  };

  services.printing.enable = true;
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    audio.enable = true;
    wireplumber.enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa.enable = true;
  };
  services.gnome.gnome-keyring.enable = true;

  users.users.qak = {
    isNormalUser = true;
    description = "Jawad Ahmed";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
  };

  nixpkgs.config.allowUnfree = true;

  environment = {
    systemPackages = with pkgs; [
      fd
      lsd
      ripgrep
      wget
      curl
      bat
      man
      nvidia-vaapi-driver
      zip
      unzip
      file
    ];
    pathsToLink = [ "/share/xdg-desktop-portal" "/share/applications" ];
  };

  xdg = {
    portal = {
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
    };
  };

  programs.fish = {
    enable = true;
    useBabelfish = true;
    shellAbbrs = {
      opam-clean = "opam clean --all-switches --download-cache --logs --repo-cache --unused-repositories";
      ghcup-gc = "ghcup gc --ghc-old --profiling-libs --share-dir --hls-no-ghc --cache --tmpdirs";
      tree = "lsd --tree";
    };
    interactiveShellInit = ''
      function paru-search
          paru --color always -Ss $argv | less -r
      end

      function paru-clean
          paru -Qtdq | paru -Rns -
          paru -Qqd  | paru -Rsu -
      end

      fish_vi_key_bindings
    '';
  };

  programs.sway = {
    enable = true;
    package = null;
    wrapperFeatures.base = false;
    extraPackages = [ ];
    extraOptions = [ ];
  };

  services.tlp.enable = true;
  services.gvfs.enable = true;
  services.udisks2.enable = true;
  services.blueman.enable = true;
  services.syncthing = {
    enable = true;
    user = "qak";
    dataDir = "/home/qak/Sync";
    systemService = true;
    settings.devices = {
      "Poco F2 Pro" = {
        id = "XZ4UOLW-7CW4ZMO-2FLVN5D-65VMWWH-WASGCRR-NDBCV2Q-ARU2T2K-YAFMDAV";
        autoAcceptFolders = true;
      };
    };
    settings.folders = {
      "obsidian-notes" = {
        path = "/home/qak/Sync/notes";
        id = "y7k6u-akw7j";
        enable = true;
        devices = [ "Poco F2 Pro" ];
      };
    };
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  system.stateVersion = "23.11";
}

