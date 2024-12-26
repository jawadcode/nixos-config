{pkgs, ...}: let
  getExe = pkgs.lib.meta.getExe;
  getExe' = pkgs.lib.meta.getExe';
in {
  home.username = "qak";
  home.homeDirectory = "/home/qak";
  home.stateVersion = "23.11";
  home.packages = with pkgs; [
    # Apps (among other things)
    audacity
    btop
    ffmpeg
    discord
    glaxnimate
    gnome-characters
    gnome-system-monitor
    imv
    kdenlive
    libreoffice-qt6-fresh
    libxml2
    kdePackages.mlt
    nemo
    nemo-fileroller
    obsidian
    papers
    qalculate-gtk
    rhythmbox
    thunderbird
    tokei
    vlc
    yt-dlp
    xorg.xlsclients
    # Fonts
    font-awesome
    hermit
    (iosevka-bin.override {variant = "SS07";})
    (callPackage ./iosevka-term-ss07-nerd-font.nix {})
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    paratype-pt-serif
    roboto
    # Language Tooling
    emacs-lsp-booster
    alejandra # Nix formatter
    elan # Can't find any convenient way to create a flake
    python312Packages.pip
    python312Packages.python
    tinymist
    tree-sitter
    typst
    nil
    pyright # Need this pretty much everywhere for writing scripts
    texlab
    # Minecraft
    temurin-jre-bin
    prismlauncher
    # Misc
    brightnessctl
    glib
    gnome-characters
    playerctl
    sway-contrib.grimshot
    swaybg
    wl-clipboard
    yaru-theme
  ];

  home.file = {
    ".config/emacs/codemacs/early-init.el".source = ./minmacs/codemacs/early-init.el;
    ".config/emacs/codemacs/init.el".source = ./minmacs/codemacs/init.el;
    ".config/emacs/mathmacs/early-init.el".source = ./minmacs/mathmacs/early-init.el;
    ".config/emacs/mathmacs/init.el".source = ./minmacs/mathmacs/init.el;
    ".config/emacs/common".source = ./minmacs/common;
    ".config/starship.toml".source = ./starship.toml;
    ".local/share/applications/codemacs.desktop".source = ./codemacs.desktop;
    ".local/share/applications/mathmacs.desktop".source = ./mathmacs.desktop;
  };

  home.sessionVariables = {};

  wayland.windowManager.sway = {
    enable = true;
    checkConfig = true;
    config = let
      terminal = "${getExe pkgs.wezterm} start";
      menu = "${getExe pkgs.wofi} --show drun --allow-markup --allow-images --prompt Application --term '${terminal}'";
    in {
      modifier = "Mod4";
      focus = {
        followMouse = true;
        mouseWarping = true;
      };
      bars = [];
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
          XF86AudioLowerVolume = "exec ${wpctl} set-volume ${id} 5%-";
          XF86AudioMute = "exec ${wpctl} set-mute ${id} toggle";

          XF86MonBrightnessUp = "exec ${brightctl} set 5%+";
          XF86MonBrightnessDown = "exec ${brightctl} set 5%-";

          XF86AudioPlay = "exec ${playerctl} play-pause";
          XF86AudioNext = "exec ${playerctl} next";
          XF86AudioPrev = "exec ${playerctl} previous";

          XF86Search = "exec ${menu}";

          "Mod4+C" = "exec ${getExe pkgs.qalculate-gtk}";
          "Mod4+Ctrl+C" = "exec ${getExe pkgs.gnome-characters}";

          "Print" = "exec ${grimshot} savecopy area";
          "Shift+Print" = "exec ${grimshot} savecopy window";
          "Ctrl+Print" = "exec ${grimshot} savecopy output";
          "Mod4+Print" = "exec ${grimshot} savecopy screen";
        };
      input = {
        "1:1:AT_Translated_Set_2_keyboard" = {xkb_layout = "gb";};
        "9610:139:SINO_WEALTH_Gaming_KB__Keyboard" = {xkb_layout = "us";};
        "1739:0:Synaptics_TM3276-022" = {
          natural_scroll = "enabled";
          tap = "enabled";
        };
      };
      output = let
        bg = resolution: mode: "${pkgs.sway}/share/backgrounds/sway/Sway_Wallpaper_Blue_${resolution}.png ${mode}";
      in {
        eDP-1 = {
          resolution = "1920x1080";
          position = "0,1080";
          bg = bg "1920x1080" "fill";
        };
        DP-1 = {
          resolution = "1920x1080";
          position = "0,0";
          bg = bg "1920x1080" "fill";
        };
        HDMI-A-2 = {
          resolution = "1280x1024";
          position = "1920,500";
          bg = bg "1920x1080" "center";
        };
      };
      floating = {
        border = 0;
        criteria = [{class = "qalculate-gtk";}];
      };
      startup = [
        {command = "${getExe pkgs.nwg-drawer} -r";}
        {command = "${getExe' pkgs.glib "gsettings"} set org.gnome.desktop.interface color-scheme prefer-dark";}
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

      workspace 1 output eDP-1 DP-1 HDMI-A-2
      workspace 2 output DP-1 HDMI-A-2 eDP-1
      workspace 3 output HDMI-A-2 eDP-1 DP-1
    '';
    wrapperFeatures.gtk = true;
    xwayland = true;
  };

  services.swayidle = let
    swaylock = getExe pkgs.swaylock;
    swaymsg = getExe' pkgs.sway "swaymsg";
  in {
    enable = true;
    events = [
      {
        event = "after-resume";
        command = "${swaymsg} 'output * power on'";
      }
      {
        event = "before-sleep";
        command = "${swaylock} -f -c 000000";
      }
    ];
    extraArgs = [
      "timeout"
      "600"
      "${swaylock} -f -c 000000"
      "timeout"
      "1200"
      "${swaymsg} 'output * power off'"
    ];
  };

  programs.wofi = {
    enable = true;
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
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
    in [
      {
        layer = "top";
        output = ["eDP-1" "DP-1"];
        position = "top";
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
        "tray" = {spacing = 10;};
        "clock" = {
          tooltip-format = ''            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
          format-alt = ''{:%Y-%m-%d}'';
        };
        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };
        "cpu" = {
          format = "{usage}% ";
          tooltip = false;
        };
        "temperature" = {
          hwmon-path-abs = "/sys/devices/platform/coretemp.0/hwmon";
          input-filename = "temp2_input";
          critical-threshold = 80;
          format = "{temperatureC}°C {icon}";
          format-icons = ["" "" ""];
        };
        "battery" = {
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
        "pulseaudio" = {
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
      }
      {
        layer = "top";
        output = ["HDMI-A-2"];
        position = "top";
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

  services.mako = {
    enable = true;
    anchor = "top-center";
    font = "sans-serif 12";
    defaultTimeout = 5000;
  };

  gtk = let
    theme = {
      package = pkgs.yaru-theme;
      name = "Yaru-blue-dark";
    };
  in {
    enable = true;
    font = {
      name = "sans-serif";
      size = 12.0;
    };
    iconTheme = theme;
    theme = theme;
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

  xdg = {
    mime.enable = true;
    mimeApps = {
      enable = true;
      defaultApplications = {
        "inode/directory" = "nemo.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "text/html" = "firefox.desktop";
        "image/gif" = "imv.desktop";
        "image/jpeg" = "imv.desktop";
        "image/png" = "imv.desktop";
        "application/ogg" = "org.gnome.Rhythmbox3.desktop";
        "audio/x-mp3" = "org.gnome.Rhythmbox3.desktop";
        "video/avi" = "vlc.desktop";
        "video/mp4" = "vlc.desktop";
        "video/webm" = "vlc.desktop";
        "text/plain" = "codemacs.desktop";
        "application/x-shellscript" = "codemacs.desktop";
        "application/pdf" = "org.gnome.Papers.desktop";
        "image/tiff" = "org.gnome.Papers.desktop";
        "application/postscript" = "org.gnome.Papers.desktop";
        "application/x-dvi" = "org.gnome.Papers.desktop";
      };
    };
    userDirs.createDirectories.enable = true;
  };

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      emoji = ["Noto Color Emoji"];
      monospace = ["Iosevka Term SS07"];
      sansSerif = ["Roboto"];
      serif = ["PT Serif"];
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
  };

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
          color_scheme = 'Apple System Colors',
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
      language-server.biome = {
        command = "biome";
        args = ["lsp-proxy"];
      };
      language = let
        common = {
          indent = {
            tab-width = 4;
            unit = "    ";
          };
          auto-format = true;
        };
        js-common = {
          language-servers = [
            {
              name = "typescript-language-server";
              except-features = ["format"];
            }
            "biome"
          ];
          inherit (common) indent auto-format;
        };
      in [
        {
          name = "python";
          language-servers = ["pyright"];
          inherit (common) indent auto-format;
        }
        {
          name = "c";
          inherit (common) indent auto-format;
        }
        {
          name = "cpp";
          inherit (common) indent auto-format;
        }
        {
          name = "javascript";
          inherit (js-common) language-servers indent auto-format;
        }
        {
          name = "typescript";
          inherit (js-common) language-servers indent auto-format;
        }
        {
          name = "tsx";
          inherit (js-common) language-servers indent auto-format;
        }
        {
          name = "jsx";
          inherit (js-common) language-servers indent auto-format;
        }
        {
          name = "json";
          inherit (js-common) language-servers indent auto-format;
        }
        {
          name = "nix";
          formatter = {
            command = "alejandra";
          };
          auto-format = true;
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

  programs.obs-studio.enable = true;

  programs.home-manager.enable = true;
}
