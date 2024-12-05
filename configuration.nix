{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-ocl
      intel-vaapi-driver
    ];
  };

  # Hoping I don't need this cos I'm using wayland, though I guess it could
  # mess with XWayland.
  services.xserver.videoDrivers = ["nvidia" "intel"];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    prime = {
      offload.enable = true;
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

  services.desktopManager.cosmic.enable = true;
  services.displayManager.cosmic-greeter.enable = true;

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
    extraGroups = ["networkmanager" "wheel"];
    shell = pkgs.fish;
  };

  environment = {
    systemPackages = with pkgs; [
      fd
      lsd
      ripgrep
      wget
      curl
      bat
      man
      zip
      unzip
      file
      nix-your-shell
      usbutils
      starship
    ];
    pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];
  };

  xdg = {
    portal = {
      wlr.enable = true;
      extraPortals = with pkgs; [xdg-desktop-portal-gtk xdg-desktop-portal-kde];
    };
  };

  environment.variables = {
    # Firefox
    MOZ_ENABLE_WAYLAND = 1;
    MOZ_USE_XINPUT2 = 1;
    # Graphics stuff
    GBM_BACKEND = "nvidia-drm";
    __GL_GSYNC_ALLOWED = 0;
    __GL_VRR_ALLOWED = 0;
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __NV_PRIME_RENDER_OFFLOAD = 1;
    __VK_LAYER_NV_optimus = "NVIDIA_only";
  };

  programs.fish = {
    enable = true;
    useBabelfish = true;
    shellAbbrs = {
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

      function fish_hybrid_key_bindings --description \
      "Vi-style bindings that inherit emacs-style bindings in all modes"
          for mode in default insert visual
              fish_default_key_bindings -M $mode
          end
          fish_vi_key_bindings --no-erase
      end

      set -g fish_key_bindings fish_hybrid_key_bindings

      source (starship init fish --print-full-init | psub)
    '';
    shellInit = ''
      nix-your-shell fish | source
    '';
  };

  # services.tlp.enable = true;
  services.gvfs.enable = true;
  services.udisks2.enable = true;
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
        devices = ["Poco F2 Pro"];
      };
    };
  };

  nix.settings = {
    experimental-features = ["nix-command" "flakes"];
    trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    substituters = [
      "https://cache.iog.io"
    ];
  };
  system.stateVersion = "23.11";
}
