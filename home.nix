{pkgs, ...}: let
  getExe = pkgs.lib.meta.getExe;
  getExe' = pkgs.lib.meta.getExe';
in {
  home.username = "qak";
  home.homeDirectory = "/home/qak";
  home.stateVersion = "24.11";
  home.packages = with pkgs; [
    # Apps (among other things)
    audacity
    btop
    ffmpeg
    discord
    evince # Just for LaTeX in emacs
    gcc # Just for tree-sitter in emacs
    glaxnimate
    gnome-system-monitor
    eog
    jetbrains-toolbox
    kdePackages.kdenlive
    lan-mouse
    libreoffice-qt6-fresh
    libxml2
    kdePackages.mlt
    mullvad
    nemo
    nemo-fileroller
    obsidian
    papers
    pavucontrol
    protonvpn-gui
    qalculate-gtk
    rhythmbox
    spotify
    thunderbird
    tokei
    ventoy
    vlc
    yt-dlp
    xorg.xlsclients
    # Fonts
    font-awesome
    ibm-plex
    (iosevka-bin.override {variant = "SS07";})
    (callPackage ./iosevka-term-ss07-nerd-font.nix {})
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    roboto
    # Language Tooling
    alejandra # Nix formatter
    comrak
    elan # Can't find any convenient way to create a flake
    emacs-all-the-icons-fonts
    emacs-lsp-booster
    python312Packages.pip
    python312Packages.python
    scrcpy
    tinymist
    tree-sitter
    typst
    nil
    pyright # Need this pretty much everywhere for writing scripts
    # Minecraft
    (prismlauncher.override {jdks = [temurin-jre-bin-17 temurin-jre-bin-21];})
    # Misc
    android-tools
    brightnessctl
    gnome-characters
    nix-your-shell
    playerctl
    sway-contrib.grimshot
    swaybg
    wofi-emoji
    wl-clipboard
    yaru-theme
  ];

  home.file = {
    # ".config/emacs/init.el".source = ./emaxx/init.el;
    # ".config/emacs/early-init.el".source = ./emaxx/early-init.el;
    ".config/starship.toml".source = ./starship.toml;
    # ".local/share/applications/emaxx.desktop".source = ./emaxx.desktop;
  };

  home.sessionVariables = {
    BAT_THEME = "OneHalfDark";
  };

  programs.fish = {
    enable = true;
    shellAbbrs = {
      tree = "lsd --tree";
      clean-crap = "nix-store --optimise && nix-collect-garbage -d && sudo nix-store --optimise && sudo nix-collect-garbage -d && sudo nixos-rebuild switch --flake ./";
    };
    interactiveShellInit = ''
      source (starship init fish --print-full-init | psub)

      function fish_hybrid_key_bindings --description \
      "Vi-style bindings that inherit emacs-style bindings in all modes"
          for mode in default insert visual
              fish_default_key_bindings -M $mode
          end
          fish_vi_key_bindings --no-erase
      end

      set -g fish_key_bindings fish_hybrid_key_bindings

      ${getExe pkgs.nix-your-shell} fish | source
    '';
  };

  wayland.windowManager.sway = {
    enable = true;
    checkConfig = true;
    config = let
      terminal = "${getExe pkgs.wezterm} -e";
      menu = getExe pkgs.wofi;
    in {
      modifier = "Mod4";
      focus = {
        followMouse = true;
        mouseWarping = true;
      };
      fonts = {
        names = ["sans-serif"];
        style = "Regular";
        size = 12.0;
      };
      gaps.inner = 5;
      inherit menu;
      keybindings = let
        wpctl = getExe' pkgs.wireplumber "wpctl";
        brightctl = getExe pkgs.brightnessctl;
        playerctl = getExe pkgs.playerctl;
        grimshot = getExe pkgs.sway-contrib.grimshot;
        id = "@DEFAULT_AUDIO_SINK@";
      in
        pkgs.lib.mkOptionDefault {
          XF86AudioRaiseVolume = "exec ${wpctl} set-volume ${id} 5%+";
          "Alt+Shift+Equal" = "exec ${wpctl} set-volume ${id} 5%+";
          XF86AudioLowerVolume = "exec ${wpctl} set-volume ${id} 5%-";
          "Alt+Shift+Minus" = "exec ${wpctl} set-volume ${id} 5%-";
          XF86AudioMute = "exec ${wpctl} set-mute ${id} toggle";

          XF86MonBrightnessUp = "exec ${brightctl} set 5%+";
          XF86MonBrightnessDown = "exec ${brightctl} set 5%-";

          XF86AudioPlay = "exec ${playerctl} play-pause";
          "Mod4+Shift+P" = "exec ${playerctl} play-pause";

          XF86AudioNext = "exec ${playerctl} next";
          "Alt+Shift+N" = "exec ${playerctl} next";

          XF86AudioPrev = "exec ${playerctl} previous";
          "Alt+Shift+P" = "exec ${playerctl} previous";

          XF86Search = "exec ${menu}";

          "Mod4+C" = "exec ${getExe pkgs.qalculate-gtk}";
          "Mod4+Ctrl+C" = "exec ${getExe pkgs.wofi-emoji}";
          "Mod4+X" = "exec ${getExe pkgs.emacs}";

          "Print" = "exec ${grimshot} savecopy area";
          "Shift+Print" = "exec ${grimshot} savecopy window";
          "Ctrl+Print" = "exec ${grimshot} savecopy output";
          "Mod4+Print" = "exec ${grimshot} savecopy screen";

          "Mod4+Shift+Delete" = "move scratchpad";
          "Mod4+Delete" = "scratchpad show";
        };
      input = {
        "1:1:AT_Translated_Set_2_keyboard" = {xkb_layout = "gb";};
        "9610:139:SINO_WEALTH_Gaming_KB__Keyboard" = {xkb_layout = "us";};
        "1739:0:Synaptics_TM3276-022" = {
          natural_scroll = "enabled";
          tap = "enabled";
        };
        "2:10:TPPS/2_IBM_TrackPoint" = {
          # accel_profile = "adaptive";
          pointer_accel = "1";
        };
      };
      output = {
        eDP-1 = {
          resolution = "1920x1080";
          position = "640,1440";
          bg = "${pkgs.sway}/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png fill";
        };
        DP-1 = {
          resolution = "2560x1440@99.946Hz";
          position = "0,0";
          bg = "${./sway-background-2560x1440.png} fill";
        };
        HDMI-A-2 = {
          resolution = "1920x1080";
          position = "2560,0";
          # position = "2560,576";
          bg = "${./sway-background-1080x1920.png} fill";
          transform = "270";
        };
      };
      workspaceOutputAssign = [
        {
          output = "eDP-1";
          workspace = "1";
        }
        {
          output = "DP-1";
          workspace = "2";
        }
        {
          output = "HDMI-A-2";
          workspace = "3";
        }
      ];

      # TV display cconfiguration
      # output = let
      #   bg = resolution: mode: "${pkgs.sway}/share/backgrounds/sway/Sway_Wallpaper_Blue_${resolution}.png ${mode}";
      # in {
      #   eDP-1 = {
      #     resolution = "1920x1080";
      #     position = "320,1440";
      #     bg = bg "1920x1080" "fill";
      #   };
      #   HDMI-A-2 = {
      #     resolution = "3840x2160@30Hz";
      #     scale = "1.5";
      #     position = "0,0";
      #     bg = "${./sway-background-3840x2160.png} fill";
      #   };
      # };

      bars = [
        {command = getExe pkgs.waybar;}
      ];

      floating = {
        border = 0;
        criteria = [{app_id = "qalculate-gtk";}];
      };
      startup = [
        {command = "${getExe pkgs.nwg-drawer} -r";}
        {command = "${getExe' pkgs.glib "gsettings"} set org.gnome.desktop.interface color-scheme prefer-dark";}
        {command = "${getExe' pkgs.glib "gsettings"} set org.gnome.desktop.interface gtk-theme Yaru-blue-dark";}
        {command = "${getExe' pkgs.glib "gsettings"} set org.gnome.desktop.interface icon-theme Yaru-blue-dark";}
      ];
      inherit terminal;
      window = {
        titlebar = false;
        border = 0;
      };
    };
    extraConfig = ''
      bindswitch lid:on output eDP-1 disable
      bindswitch lid:off output eDP-1 enable

      bindgesture swipe:3:right workspace prev
      bindgesture swipe:3:left workspace next

      workspace 1 output eDP-1
      workspace 2 output DP-1
      workspace 3 output HDMI-A-2
    '';
    extraOptions = ["--unsupported-gpu"];
    wrapperFeatures.gtk = true;
    xwayland = true;
  };

  home.pointerCursor = {
    enable = true;
    package = pkgs.posy-cursors;
    dotIcons.enable = true;
    gtk.enable = true;
    name = "Posy_Cursor_Black";
    sway.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "left_ptr";
    };
  };

  services.swayidle = let
    swaylock = getExe pkgs.swaylock;
  in {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${swaylock} -f -c 000000";
      }
    ];
    extraArgs = [
      "-w"
      "timeout"
      "600"
      "${swaylock} -f -c 000000"
      "timeout"
      "1200"
      "systemctl hibernate"
    ];
  };

  programs.wofi = {
    enable = true;
    settings = {
      show = "drun";
      allow_markup = true;
      allow_images = true;
      prompt = "Application";
      term = "wezterm -e";
      width = 640;
    };
  };
  # services.walker = {
  #   enable = true;
  #   systemd.enable = true;
  # };

  xdg = {
    mime.enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = "nemo.desktop";
        "text/html" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
        "image/gif" = "imv.desktop";
        "image/jpeg" = "imv.desktop";
        "image/png" = "imv.desktop";
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

  programs.waybar = {
    enable = true;
    systemd.enable = false; # Cus of cosmic
    settings = let
      "sway/mode" = {
        format = ''<span style="italic">{}</span>'';
      };
      # I have no idea what this is lol
      "sway/scratchpad" = {
        format = "{icon} {count}";
        show-empty = false;
        format-icons = ["" ""];
        tooltip = true;
        tooltip-format = "{app} = {title}";
      };
      mkBar = outputs: {
        output = outputs;
        height = 32;
        spacing = 4;
        modules-left = [
          "sway/workspaces"
          "sway/mode"
          "sway/scratchpad"
        ];
        modules-center = ["sway/window"];
        modules-right = [
          "idle_inhibitor"
          "pulseaudio"
          "cpu"
          "temperature"
          "battery"
          "tray"
          "clock"
        ];
        inherit "sway/mode";
        inherit "sway/scratchpad";
        tray = {spacing = 10;};
        clock = {
          tooltip-format = ''            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
          format-alt = ''{:%Y-%m-%d}'';
        };
        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };
        cpu = {
          format = "{usage}% ";
          tooltip = false;
        };
        temperature = {
          hwmon-path-abs = "/sys/devices/platform/coretemp.0/hwmon";
          input-filename = "temp2_input";
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = ["" "" ""];
        };
        battery = {
          states = {
            good = 95;
            warning = 20;
            critical = 10;
          };
          format = "{capacity}% {icon}";
          format-full = "{capacity}% {icon}";
          format-charging = "{capacity}% ";
          format-plugged = "{capacity}% ";
          format-alt = "{time} {icon}";
          format-icons = ["" "" "" "" ""];
        };
        pulseaudio = {
          format = "{volume}% {icon} {format_source}";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " {format_source}";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "pavucontrol";
        };
      };
    in [
      (mkBar ["eDP-1"])
      ((mkBar ["DP-1"])
        // {
          modules-right = [
            "idle_inhibitor"
            "cpu"
            "memory"
            "network"
            "temperature"
            "battery"
            "tray"
            "clock"
          ];
          memory.format = "{used}/{total}GiB ({percentage}%) ";
          network = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ipaddr}/{cidr} ";
            tooltip-format = "{ifname} via {gwaddr} ";
            format-linked = "{ifname} (No IP) ";
            format-disconnected = "Disconnected ⚠";
            format-alt = "{ifname}= {ipaddr}/{cidr}";
          };
        })
      {
        # layer = "top";
        output = ["HDMI-A-2"];
        # position = "top";
        height = 32;
        spacing = 4;
        modules-left = ["sway/workspaces" "sway/mode" "sway/scratchpad"];
        modules-center = ["sway/window"];
        modules-right = [];
        inherit "sway/mode";
        inherit "sway/scratchpad";
      }
    ];
    style = ./waybar-style.css;
  };

  services.blueman-applet.enable = true;

  services.mako = {
    enable = true;
    settings = {
      anchor = "top-center";
      font = "sans-serif 12";
      default-timeout = 5000;
    };
  };

  gtk = let
    theme = {
      package = pkgs.yaru-theme;
      name = "Yaru-blue-dark";
    };
  in {
    enable = true;
    inherit theme;
    font = {
      name = "sans-serif";
      size = 12.0;
    };
    gtk3.extraConfig.gtk-theme-name = theme.name;
    gtk4.extraConfig.gtk-theme-name = theme.name;
    gtk3.extraConfig.gtk-application-prefer-dark-theme = 1;
    gtk4.extraConfig.gtk-application-prefer-dark-theme = 1;
  };

  programs.git = let
    email = "jawad.w.ahmed@gmail.com";
  in {
    enable = true;
    package = pkgs.gitFull;
    userEmail = email;
    userName = "jawadcode";
    extraConfig = {
      init.defaultBranch = "master";
      author.name = "Jawad W. Ahmed";
      author.email = email;
      credential.helper = "${pkgs.gitFull}/bin/git-credential-libsecret";
    };
  };

  programs.nix-index.enable = true;

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      emoji = ["Noto Color Emoji"];
      monospace = ["Iosevka Term SS07"];
      sansSerif = ["Roboto"];
      serif = ["IBM Plex Serif"];
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = epkgs: with epkgs; [treesit-grammars.with-all-grammars];
  };

  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  #   defaultEditor = true;
  #   extraOptions = ["--init-directory" "~/.config/emacs"];
  #   startWithUserSession = "graphical";
  # };

  programs.firefox = {
    enable = true;
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisplayBookmarksToolbar = "newtab";
      OfferToSaveLogins = false;
      HardwareAcceleration = true;
      EnableTrackingProtection = {
        Value = true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
        Exceptions = [];
      };
      NoDefaultBookmarks = true;
      PromptForDownloadLocation = true;
      AutofillCreditCardEnabled = true;
    };
  };

  programs.wezterm = {
    enable = true;
    extraConfig = ''
      return {
          front_end = "WebGpu",
          enable_wayland = false,
          color_scheme = "Apple System Colors",
          font = wezterm.font_with_fallback({ "IosevkaTermSS07 Nerd Font", "Noto Color Emoji" }),
          font_size = 13.5,
          hide_tab_bar_if_only_one_tab = true,
      }
    '';
  };

  programs.helix = {
    enable = true;
    defaultEditor = true;
    languages = {
      language = let
        applyCommon = lang:
          {
            indent = {
              tab-width = 4;
              unit = "    ";
            };
            auto-format = true;
          }
          // lang;
      in
        map
        applyCommon
        [
          {
            name = "python";
            language-servers = ["pyright"];
          }
          {
            name = "c";
          }
          {
            name = "cpp";
          }
        ]
        ++ [
          {
            name = "nix";
            formatter = {
              command = "alejandra";
            };
          }
        ];
    };
    settings = {
      theme = "dark_plus";
      editor = {
        bufferline = "always";
        lsp.display-inlay-hints = true;
        statusline = {
          left = ["mode" "spinner"];
          center = ["file-name"];
          right = ["diagnostics" "selections" "position" "file-encoding" "file-line-ending" "file-type"];
          separator = "│";
        };
      };
      keys.normal = {
        "S-tab" = "goto_next_buffer";
        "A-tab" = "goto_previous_buffer";
      };
    };
  };

  programs.vscode = {
    enable = true;
    mutableExtensionsDir = false;
    profiles.default = {
      userSettings = {
        "editor.fontFamily" = "'Iosevka Term SS07'";
        "editor.fontSize" = 18;
        "rust-analyzer.server.path"= "rust-analyzer";
      };
      extensions = with pkgs.vscode-extensions; [
        astro-build.astro-vscode
        mkhl.direnv
        rust-lang.rust-analyzer
        myriad-dreamin.tinymist
        ocamllabs.ocaml-platform
        svelte.svelte-vscode
        vscodevim.vim
      ];
      enableExtensionUpdateCheck = false;
      enableUpdateCheck = false;
    };
  };

  programs.home-manager.enable = true;
}
