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

  # Taken from https://github.com/NixOS/nixos-hardware/blob/master/common/gpu/intel/kaby-lake/default.nix
  boot.kernelParams = [
    "i915.enable_guc=2"
    "i915.enable_fbc=1"
    "i915.enable_psr=2"
  ];

  hardware.enableAllFirmware = true;
  hardware.graphics.enable = true;

  console.keyMap = "uk";

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    open = false;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    modesetting.enable = true;
    powerManagement = {
      enable = false;
      finegrained = false;
    };
    prime = {
      sync.enable = true;
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
  services.throttled.enable = true;

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
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
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
      usbutils
      starship
    ];
    pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];
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
    # I don't use prime render offload anymore since apparently my GPU isn't
    # supported.
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
  };

  programs.command-not-found.enable = false;

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
      options = "--delete-older-than 1w";
    };
    settings = {
      experimental-features = ["nix-command" "flakes"];
      trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      ];
      substituters = [
        "https://cache.iog.io"
      ];
      auto-optimise-store = true;
    };
  };
  system.stateVersion = "24.11";
}
