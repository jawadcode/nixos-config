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

  hardware.enableAllFirmware = true;
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-ocl
      intel-vaapi-driver
    ];
  };

  console.keyMap = "uk";

  # Hoping I don't need this cos I'm using wayland, though I guess it could help with XWayland.
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

  services.tlp = {
    enable = true;
    settings = {
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_performance";

      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "balanced";

      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;

      CPU_HWP_DYN_BOOST_ON_AC = 1;
      CPU_HWP_DYN_BOOST_ON_BAT = 0;
    };
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

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

  # services.desktopManager.cosmic.enable = true;
  # services.displayManager.cosmic-greeter.enable = true;

  services.displayManager = {
    sddm = {
      enable = true;
      wayland.enable = true;
    };
    sessionPackages = [pkgs.sway];
    defaultSession = "sway";
  };

  programs.sway = {
    enable = true;
    package = null;
    wrapperFeatures.base = false;
  };

  services.printing.enable = true;
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
      dconf
      fd
      lsd
      ripgrep
      wget
      curl
      bat
      man
      man-pages
      man-pages-posix
      zip
      unzip
      file
      ntfs3g
      nix-your-shell
      usbutils
      starship
    ];
    pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];
  };

  xdg = {
    portal = {
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
    # Wayland Stuff
    NIXOS_OZONE_WL = 1;
  };

  documentation = {
    dev.enable = true;
    man = {
      man-db.enable = false;
      mandoc.enable = true;
    };
  };

  programs.fish = {
    enable = true;
    useBabelfish = true;
    shellAbbrs = {
      tree = "lsd --tree";
    };
    interactiveShellInit = ''
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

  nix = {
    gc = {
      dates = "weekly";
      automatic = true;
    };
    settings = {
      experimental-features = ["nix-command" "flakes"];
      trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      substituters = [
        "https://cache.iog.io"
      ];
    };
  };
  system.stateVersion = "23.11";
}
