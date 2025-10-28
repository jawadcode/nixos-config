{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot = {
    loader.systemd-boot = {
      enable = true;
      extraInstallCommands = ''
        default_cfg=$(${pkgs.coreutils}/bin/cat /boot/loader/loader.conf | ${pkgs.gnugrep}/bin/grep default | ${pkgs.gawk}/bin/awk '{print $2}')
        tmp=$(${pkgs.coreutils}/bin/mktemp -d)

        ${pkgs.coreutils}/bin/echo -ne "$default_cfg\0" | ${pkgs.iconv}/bin/iconv -f utf-8 -t utf-16le > $tmp/efivar.txt

        ${pkgs.efivar}/bin/efivar -n 4a67b082-0a4c-41cf-b6c7-440b29bb8c4f-LoaderEntryLastBooted -w -f $tmp/efivar.txt
        ${pkgs.systemd}/bin/bootctl set-default @saved
      '';
      extraEntries = {
        "arch-grub.conf" = ''
          title Arch (GRUB)
          efi /efi/ARCH-GRUB/grubx64.efi
        '';
      };
    };
    loader.efi.canTouchEfiVariables = true;

    # Taken from https://github.com/NixOS/nixos-hardware/blob/master/common/gpu/intel/kaby-lake/default.nix
    kernelParams = [
      "i915.enable_guc=2"
      "i915.enable_fbc=1"
      "i915.enable_psr=2"
      # "nvidia_drm.modeset=1"
      # "nvidia_drm.fbdev=1"
      # "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
    ];

    plymouth.enable = true;
    tmp.cleanOnBoot = true;
  };

  hardware.enableAllFirmware = true;
  hardware.graphics.enable = true;

  console.keyMap = "uk";

  # Tested these values in ThrottleStop on Windows 11.
  services.undervolt = {
    enable = true;
    coreOffset = -90;
    gpuOffset = -40;
  };

  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    open = false;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    powerManagement.enable = true;
    modesetting.enable = true;
    prime = {
      offload.enable = true;

      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  services.tlp = {
    enable = true;
    settings = {
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;

      START_CHARGE_THRESH_BAT1 = 75;
      STOP_CHARGE_THRESH_BAT1 = 80;

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

  services.timesyncd.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;

  networking.hostName = "ixnay";
  networking.networkmanager.enable = true;
  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  time.timeZone = "Europe/London";

  i18n = {
    defaultLocale = "en_GB.UTF-8";
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

  xdg = {
    portal = {
      enable = true;
      wlr.enable = true;
      xdgOpenUsePortal = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
    };
    terminal-exec = {
      enable = true;
      settings.default = ["org.wezfurlong.wezterm"];
    };
  };

  services.printing = {
    enable = true;
    drivers = [pkgs.hplip];
  };
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
      efivar
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
      xdg-utils
      xdg-user-dirs
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
    __NV_PRIME_RENDER_OFFLOAD = 1;
    __NV_PRIME_RENDER_OFFLOAD_PROVIDER = "NVIDIA_G0";
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
    # settings.devices = {
    #   "Pixel 9" = {
    #     id = "RH3N6IR-ATFE5MF-LMK56NY-XC4FLSR-6B3LRXC-XS5SOH4-SE3L2IB-HZMSCQF";
    #     autoAcceptFolders = true;
    #   };
    #   "hp-sauce" = {
    #     id = "4WNPP3A-PBYNO7O-5J64I65-2LV4U3N-TAZCASN-VTMXSN7-7F2H2ZZ-BXXYXA2";
    #     autoAcceptFolders = true;
    #   };
    # };
    # settings.folders = {
    #   "obsidian-notes" = {
    #     path = "/home/qak/Sync/notes";
    #     id = "y7k6u-akw7j";
    #     enable = true;
    #     devices = ["Pixel 9" "hp-sauce"];
    #   };
    #   "mc-untitled-world-1" = {
    #     path = "/home/qak/.local/share/PrismLauncher/instances/vanilla/minecraft/saves/Untitled World #1";
    #     id = "mc-untitled-world-1";
    #     enable = true;
    #     devices = ["Pixel 9" "hp-sauce"];
    #   };
    # };
  };

  programs.obs-studio.enable = true;

  nix = {
    gc = {
      dates = "weekly";
      automatic = true;
      options = "--delete-older-than 7d";
    };
  };
  system.stateVersion = "24.11";
}
