{
  lib,
  pkgs,
  ...
}: {
  imports = [./hardware-configuration.nix];

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        # I don't even know where this is from or how it works, but it sets the default boot option to the most recently booted entry.
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
        # };
      };
      efi.canTouchEfiVariables = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
    plymouth.enable = true;
    tmp.cleanOnBoot = true;
  };

  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
    enableAllFirmware = true;
    graphics.enable = true;
  };

  security = {
    polkit.enable = true;
    pam.services = {
      greetd.enableGnomeKeyring = true;
      swaylock.enableGnomeKeyring = true;
    };
  };

  systemd.user.services.kanshi = {
    description = "kanshi daemon";
    environment = {
      WAYLAND_DISPLAY = "wayland-1";
      DISPLAY = ":0";
    };
    # requires = ["graphical.target"];
    # after = ["graphical.target"];
    serviceConfig = {
      Type = "simple";
      # ExecStartPre = ''${lib.meta.getExe' pkgs.coreutils-full "sleep"} 5'';
      ExecStart = ''${lib.meta.getExe pkgs.kanshi} -c /home/qak/.config/kanshi/config'';
    };
  };

  networking = {
    hostName = "allbuch-nix";
    networkmanager.enable = true;
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  services.blueman.enable = true;

  time.timeZone = "Europe/London";
  services.timesyncd.enable = true;

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

  services.xserver.xkb = {
    layout = "gb";
    variant = "";
  };
  console.keyMap = "uk";

  users.users.qak = {
    isNormalUser = true;
    description = "Jawad Ahmed";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    shell = pkgs.fish;
    packages = with pkgs; [
      nix-your-shell
      xdg-utils
      xdg-user-dirs
      zip
      unzip
      usbutils
      file
      lsd
      bat
      btop
      ffmpeg
      yt-dlp
      streamlink
      tokei
      gcc

      nil
      alejandra
      pyright
      black
      bash-language-server
      shellcheck
      shfmt

      yaru-theme

      (nemo-with-extensions.override {
        extensions = [
          nemo-preview
          nemo-fileroller
        ];
      })
      loupe
      vlc
      gnome-calculator
      rhythmbox
      discord
      spotify
      obsidian
      vscode
      protonvpn-gui
      kdePackages.kdenlive
      (prismlauncher.override {
        jdks = [
          temurin-jre-bin-17
          temurin-jre-bin-21
        ];
      })

      nerd-fonts.symbols-only
      sqlite
      wordnet
      ispell
      go-grip
      emacs-lsp-booster
      ((emacsPackagesFor emacs-pgtk).emacsWithPackages (epkgs: [
        # epkgs.treesit-grammars.with-all-grammars
      ]))
      jetbrains-toolbox
    ];
  };

  environment = {
    systemPackages = with pkgs; [
      ntfs3g
      efivar
      man
      man-pages
      man-pages-posix
      wget
      curl
      git
      ripgrep
      fd
      helix

      glib
      xdg-utils
    ];
    variables = {
      MOZ_ENABLE_WAYLAND = 1;
      MOZ_USE_XINPUT2 = 1;
      NIXOS_OZONE_WL = 1;
      EDITOR = "hx";
    };
  };

  programs.fish = {
    enable = true;
    shellAbbrs = {
      tree = "lsd --tree";
      clean-crap = "nix-store --optimise && nix-collect-garbage -d && sudo nix-store --optimise && sudo nix-collect-garbage -d && sudo nixos-rebuild switch --flake ~/nixos-config";
    };
    shellAliases = {
      grip = lib.meta.getExe pkgs.go-grip;
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

      ${lib.getExe pkgs.nix-your-shell} fish | source
    '';
  };
  programs.direnv.enable = true;
  programs.nix-index.enable = true;
  programs.starship = {
    enable = true;
    settings = {
      character = {
        success_symbol = "[λx.](bold blue)";
        error_symbol = "[λx.](bold red)";
      };
    };
    presets = ["bracketed-segments"];
  };

  services.gnome = {
    gnome-keyring.enable = true;
    gcr-ssh-agent.enable = true;
  };
  programs.seahorse.enable = true;

  services.printing.enable = true;

  documentation = {
    dev.enable = true;
    man = {
      man-db.enable = false;
      mandoc.enable = true;
    };
  };

  services.tailscale.enable = true;

  programs.git = {
    enable = true;
    config = let
      email = "jawad.w.ahmed@gmail.com";
    in {
      author = {
        inherit email;
        name = "Jawad W. Ahmed";
      };
      user = {
        inherit email;
        name = "jawadcode";
      };
      credential.helper = "${pkgs.gitFull}/bin/git-credential-libsecret";
    };
  };

  services.gvfs.enable = true;
  services.udisks2.enable = true;
  programs.gnome-disks.enable = true;

  # services.displayManager = {
  #   enable = true;
  #   # sddm = {
  #   #   enable = true;
  #   #   wayland.enable = true;
  #   # };
  # };

  services.greetd.enable = true;
  programs.regreet = {
    enable = true;
    cageArgs = ["-s" "-m" "last"];
  };

  programs.dconf.profiles.user = {
    databases = [
      {
        settings = {
          "org/gnome/desktop/interface" = {
            color-scheme = "prefer-dark";
            gtk-theme = "Yaru-blue-dark";
            icon-theme = "Yaru-blue-dark";
            cursor-theme = "Posy's Cursor Black";
          };
        };
      }
    ];
  };

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    xwayland.enable = true;
    extraPackages = with pkgs; [
      brightnessctl
      wezterm
      sway-contrib.grimshot
      swayidle
      swaylock
      ulauncher
      pavucontrol
      playerctl
      mako
      wl-clipboard
      posy-cursors
    ];
  };

  services.playerctld.enable = true;

  programs.waybar.enable = true;

  xdg = {
    portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
      xdgOpenUsePortal = true;
    };
    terminal-exec = {
      enable = true;
      settings.default = ["org.wezfurlong.wezterm"];
    };
    mime = {
      enable = true;
      defaultApplications = {
        "inode/directory" = "nemo.desktop";
        "text/html" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
        "image/gif" = "org.gnome.Loupe";
        "image/jpeg" = "org.gnome.Loupe";
        "image/png" = "org.gnome.Loupe";
        "application/ogg" = "org.gnome.Rhythmbox3.desktop";
        "audio/x-mp3" = "org.gnome.Rhythmbox3.desktop";
        "video/avi" = "vlc.desktop";
        "video/mp4" = "vlc.desktop";
        "video/webm" = "vlc.desktop";
        "text/plain" = "emacs.desktop";
        "application/x-shellscript" = "emacs.desktop";
        "application/pdf" = "org.gnome.Evince.desktop";
        "image/tiff" = "org.gnome.Evince.desktop";
        "application/postscript" = "org.gnome.Evince.desktop";
        "application/x-dvi" = "org.gnome.Evince.desktop";
      };
    };
  };

  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    alsa.enable = true;
    jack.enable = true;
    pulse.enable = true;
  };

  services.syncthing = {
    enable = true;
    user = "qak";
    dataDir = "/home/qak/.sync";
    systemService = true;
  };

  fonts = {
    packages = with pkgs; [
      ibm-plex
      roboto
      (iosevka-bin.override {variant = "SS07";})
      noto-fonts
      noto-fonts-color-emoji
      font-awesome
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        sansSerif = ["Roboto"];
        serif = ["IBM Plex Serif"];
        monospace = ["Iosevka Term SS07"];
        emoji = ["Noto Color Emoji"];
      };
    };
  };

  services.gnome.evolution-data-server.enable = true;
  programs.evolution.enable = true;
  programs.evince.enable = true;

  programs.firefox = {
    enable = true;
    preferences = {
      "browser.compactmode.show" = true;
    };
    policies = {
      Cookies.Behavior = "reject-foreign";
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisplayBookmarksToolbar = "newtab";
      DontCheckDefaultBrowser = true;
      EnableTrackingProtection = true;
      FirefoxHome = {
        Search = true;
        TopSites = false;
        SponsoredTopSites = false;
        Highlights = false;
        Pocket = false;
        SponsoredPocket = false;
        Locked = false;
      };
      HardwareAcceleration = true;
      NoDefaultBookmarks = true;
      OfferToSaveLogins = false;
      PasswordManagerEnabled = false;
      SearchEngines = {
        Add = [
          {
            Name = "Startpage";
            URLTemplate = "https://www.startpage.com/sp/search?query={searchTerms}&abp=0&abe=0&t=device&lui=english&sc=w47g7LtL57nG20&cat=web&abd=0&abe=0&prfe=48b70904004aaca120d6c9c9ab32a11c0ab13aae089bf2b57993bd9f7422ba9de2a6b22f602a52adfc5ab8316fce6c3aa5c828b804229ffbe1af9e4ba5b9eb3d666088d848857df6c7f2fd13";
            Method = "GET";
            IconURL = "https://www.startpage.com/favicon.ico";
            Alias = "startpage";
            Description = "Startpage Search";
            SuggestURLTemplate = "https://www.startpage.com/osuggestions?q={searchTerms}";
          }
        ];
        Default = "Startpage";
      };
      ExtensionSettings = {
        "uBlock0@raymondhill.net" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
          private_browsing = true;
        };
        "{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/latest.xpi";
          private_browsing = true;
        };
        "{d7742d87-e61d-4b78-b8a1-b469842139fa}" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/vimium-ff/latest.xpi";
          private_browsing = true;
        };
        "jid1-MnnxcxisBPnSXQ@jetpack" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/privacy-badger17/latest.xpi";
          private_browsing = true;
        };
        "tab-stash@condordes.net" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/tab-stash/latest.xpi";
          private_browsing = true;
        };
        "{762f9885-5a13-4abd-9c77-433dcd38b8fd}" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/return-youtube-dislikes/latest.xpi";
          private_browsing = true;
        };
        "izer@camelcamelcamel.com" = {
          installation_mode = "normal_installed";
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/the-camelizer-price-history-ch/latest.xpi";
          private_browsing = true;
        };
      };
      ExtensionUpdate = true;
    };
  };

  programs.obs-studio.enable = true;

  system.stateVersion = "25.05";
}
