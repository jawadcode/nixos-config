{ config, pkgs, ... }:

{
  home.username = "qak";
  home.homeDirectory = "/home/qak";
  home.stateVersion = "23.11";
  home.packages = with pkgs; [
    # Sway Utilities
    brightnessctl nwg-drawer sway-contrib.grimshot swaybg swayidle swaylock
    wl-clipboard
    # Apps
    cinnamon.nemo discord evince gnome.eog gnome.file-roller
    gnome.gnome-disk-utility gnome.gnome-characters gnome.gnome-system-monitor
    helix kdePackages.kdenlive obsidian playerctl qalculate-gtk rhythmbox
    thunderbird vlc yt-dlp
    # Fonts
    font-awesome hermit (iosevka-bin.override { variant = "SS07"; })
    (callPackage ./iosevka-term-ss07-nerd-font.nix {}) noto-fonts noto-fonts-cjk
    noto-fonts-color-emoji roboto
    # Language Toolchains
    clang cmake elan gnumake haskellPackages.haskell-language-server
    haskellPackages.ormolu haskellPackages.stack idris2 lldb meson mold ninja
    nodePackages.nodejs opam python312Packages.pip python312Packages.python
    texliveFull typescript
    # LSPs
    clang-tools cmake-language-server idris2Packages.idris2Lsp
    nil nodePackages.svelte-language-server
    nodePackages.typescript-language-server pyright vscode-langservers-extracted
    texlab
  ];

  home.file = {
    ".config/emacs/early-init.el".source = ./emacs/early-init.el;
    ".config/emacs/init.el".source = ./emacs/init.el;
    ".config/electron-flags.conf".source = ./electron-flags.conf;
  };

  home.sessionVariables = {};

  programs.git = let email = "jawad.w.ahmed@gmail.com"; in {
    enable = true;
    package = pkgs.gitFull;
    userEmail = email;
    userName = "jawadcode";
    extraConfig = {
      author.name = "Jawad W. Ahmed";
      author.email = email; 
      credential.helper = "${ pkgs.gitFull }/bin/git-credential-libsecret";
    };
  };

  fonts = {
    fontconfig.defaultFonts = {
      monospace = [ "Iosevka Term SS07" ];
      sansSerif = [ "Roboto" "Noto Sans Arabic" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  xdg.enable = true;

  programs.waybar = {
    enable = true;
    # systemd.enable = true;
    settings =
      let
        "sway/mode" = {
          format = ''<span style="italic">{}</span>'';
        };
        # I have no idea what this is lol
        "sway/scratchpad" = {
          format = "{icon} {count}";
          show-empty = false;
          format-icons = [ "" "" ];
          tooltip = true;
          tooltip-format = "{app} = {title}";
        };
      in [
        {
          layer = "top";
          output = [ "eDP-1" "DP-1" ];
          position = "top";
          height = 32;
          spacing = 4;
          modules-left = [
            "sway/workspaces"
            "sway/mode"
            "sway/scratchpad"
          ];
          modules-center = [ "sway/window" ];
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
          "tray" = { spacing = 10; };
          "clock" = {
            tooltip-format = ''<big>{:%Y %B}</big>
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
            format-icons = [ "" "" "" ];
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
            format-icons = [ "" "" "" "" "" ];
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
              default = [ "" "" "" ];
            };
            on-click = "pavucontrol";
          };
        }
        {
          layer = "top";
          output = [ "HDMI-A-2" ];
          position = "top";
          height = 32;
          spacing = 4;
          modules-left = [
            "sway/workspaces"
            "sway/mode"
            "sway/scratchpad"
          ];
          modules-center = [ "sway/window" ];
          modules-right = [];
          inherit "sway/mode";
          inherit "sway/scratchpad";
        }
      ];
    style =
''
window {
  font-family: sans-serif, "Font Awesome 6 Free";
  font-size: 16px;
}

/* Purposely garish, cope */
window#waybar {
  background-color: rgba(36, 36, 36, 0.7);
  border-bottom: 3px solid rgb(64, 64, 64);
  color: #E0E0E0;
  transition-property: background-color;
  transition-duration: .5s;
}

window#waybar.hidden {
  opacity: 0.2;
}

button {
  box-shadow: inset 0 -3px transparent;
  border: none;
  border-radius: 0;
}

button:hover {
  background: inherit;
  box-shadow: inset 0 -3px #ffffff;
}

#pulseaudio:hover {
  background-color: #a37800;
}

#workspaces button {
  padding: 0 5px;
  background-color: transparent;
  color: #ffffff;
}

#workspaces button:hover {
  background: rgba(0, 0, 0, 0.2);
}

#workspaces button.focused {
  background-color: #64727D;
  box-shadow: inset 0 -3px #ffffff;
}

#workspaces button.urgent {
  background-color: #eb4d4b;
}

#mode {
  background-color: #64727D;
  box-shadow: inset 0 -3px #ffffff;
}

#scratchpad,
#mode,
#idle_inhibitor,
#pulseaudio,
#cpu,
/* #memory, */
#temperature,
#battery,
#tray,
#clock {
  padding: 0 10px;
  margin: 3px 0;
  color: #E0E0E0;
}

#window,
#workspaces {
  margin: 0 4px;
}

/* If workspaces is the leftmost module, omit left margin */
.modules-left>widget:first-child>#workspaces {
  margin-left: 0;
}

/* If workspaces is the rightmost module, omit right margin */
.modules-right>widget:last-child>#workspaces {
  margin-right: 0;
}

#clock {
  background-color: #2980b9;
  margin-right: 3px;
}

#battery {
  background-color: #ffffff;
  color: #202020;
}

#battery.charging,
#battery.plugged {
  color: #ffffff;
  background-color: #26A65B;
}

@keyframes blink {
  to {
    background-color: #ffffff;
    color: #202020;
  }
}

/* Using steps() instead of linear as a timing function to limit cpu usage */
#battery.critical:not(.charging) {
  background-color: #f53c3c;
  color: #ffffff;
  animation-name: blink;
  animation-duration: 0.5s;
  animation-timing-function: steps(12);
  animation-iteration-count: infinite;
  animation-direction: alternate;
}

label:focus {
  background-color: #202020;
}

#idle_inhibitor {
  background-color: #2d3436;
}

#idle_inhibitor.activated {
  background-color: #ecf0f1;
  color: #2d3436;
}

#pulseaudio {
  background-color: #f1c40f;
  color: #202020;
}

#cpu {
  background-color: #2ecc71;
  color: #202020;
}

/* #memory {
    background-color: #9b59b6;
} */

#disk {
  background-color: #964B00;
}

#network {
  background-color: #2980b9;
}

#network.disconnected {
  background-color: #f53c3c;
}

#pulseaudio.muted {
  background-color: #90b1b1;
  color: #2a5c45;
}

#temperature {
  background-color: #d0730b;
}

#temperature.critical {
  background-color: #eb4d4b;
}

#tray {
  background-color: #64727D;
}

#tray>.passive {
  -gtk-icon-effect: dim;
}

#tray>.needs-attention {
  -gtk-icon-effect: highlight;
  background-color: #eb4d4b;
}

#scratchpad {
  background: rgba(0, 0, 0, 0.2);
}

#scratchpad.empty {
  background-color: transparent;
}

#privacy {
  padding: 0;
}

#privacy-item {
  padding: 0 5px;
  color: white;
}

#privacy-item.screenshare {
  background-color: #cf5700;
}

#privacy-item.audio-in {
  background-color: #1ca000;
}

#privacy-item.audio-out {
  background-color: #0069d4;
}
'';
  };

  wayland.windowManager.sway = {
    enable = true;
    config =
      let
        findExe = pkg: pkgs.lib.meta.getExe pkg;
        terminal = "${findExe pkgs.wezterm} -e";
        menu = ''nwg-drawer -fm "${findExe pkgs.cinnamon.nemo}" -term "${terminal}"'';
      in {
        modifier = "Mod4";
        bars = [{
          command = "${findExe pkgs.waybar}";
          position = "top";
        }];
        focus = {
          followMouse = true;
          mouseWarping = true;
        };
        fonts = {
          names = [ "sans-serif" ];
          style = "Regular";
          size = 12.0;
        };
        gaps.inner = 5;
        inherit menu;
        keybindings =
          let
            mod = config.wayland.windowManager.sway.config.modifier;
            wpctl = pkgs.lib.meta.getExe' pkgs.wireplumber "wpctl";
            brightctl = findExe pkgs.brightnessctl;
            playerctl = findExe pkgs.playerctl;
            grimshot = findExe pkgs.sway-contrib.grimshot;
            id = "@DEFAULT_AUDIO_SINK@";
          in pkgs.lib.mkOptionDefault {
            "XF86AudioRaiseVolume" = "exec ${wpctl} set-volume ${id} 5%+";
            "XF86AudioLowerVolume" = "exec ${wpctl} set-volume ${id} 5%-";
            "XF86AudioMute" = "exec ${wpctl} set-mute ${id} toggle";

            "XF86MonBrightnessUp" = "exec ${brightctl} set 5%+";
            "XF86MonBrightnessDown" = "exec ${brightctl} set 5%-";

            "XF86AudioPlay" = "exec ${playerctl}  play-pause";
            "XF86AudioNext" = "exec ${playerctl} next";
            "XF86AudioPrev" = "exec ${playerctl} previous";

            "XF86Search" = "exec ${menu}";

            "Mod4+Q" = "exec ${findExe pkgs.qalculate-gtk}";
            "Mod4+Ctrl+C" = "exec ${findExe pkgs.gnome.gnome-characters}";

            "Print" = "exec ${grimshot} savecopy area";
            "Shift+Print" = "exec ${grimshot} savecopy window";
            "Ctrl+Print" = "exec ${grimshot} savecopy output";
            "Ctrl+Shift+Print" = "exec ${grimshot} savecopy screen";
          };
        output = let bg = "${ ./assets/sway-bg.png } fill"; in {
          DP-1 = { resolution = "1920x1080"; position = "0,0"; inherit bg; };
          eDP-1 = { resolution = "1920x1080"; position = "0,1080"; inherit bg; };
          HDMI-A-2 = { resolution = "1280x1024"; position = "1920,450"; inherit bg; };
        };
        input = {
          "1:1:AT_Translated_Set_2_keyboard" = { xkb_layout = "gb"; };
          # Add US-layout mech kb
          "9610:139:SINO_WEALTH_Gaming_KB__Keyboard" = { xkb_layout = "us"; };
          "1739:0:Synaptics_TM3276-022" = { natural_scroll = "enabled"; tap = "enabled"; };
        };
        floating.criteria = [{ class = "qalculate-gtk"; }];
        startup = [
          { command = "nwg-drawer -r"; }
          { command =
  ''swayidle -w \
           timeout 300 'swaylock -f -c 000000' \
           timeout 600 'swaymsg "output * power off"' resume 'swaymsg "output * power on"' \
           before-sleep 'swaylock -f -c 000000'
  ''; }
        ];
        inherit terminal;
        window = {
          titlebar = false;
          border = 0;
        };
      };
    extraSessionCommands = ''
      export LIBVA_DRIVER_NAME=nvidia
      export XDG_SESSION_TYPE=wayland
      export GBM_BACKEND=nvidia-drm
      # export __GLX_VENDOR_LIBRARY_NAME=nvidia
      export __NV_PRIME_RENDER_OFFLOAD=1
      export __VK_LAYER_NV_optimus=NVIDIA_only

      export SDL_VIDEODRIVER=wayland
      export GTK_BACKEND=wayland
      export QT_QPA_PLATFORM=wayland
      export MOZ_ENABLE_WAYLAND=1

      export GTK_THEME=Yaru-blue-dark
    '';
    extraOptions = [ "--unsupported-gpu" ];
    extraConfig = ''
      bindswitch lid:on output eDP-1 disable
      bindswitch lid:off output eDP-1 enable

      bindgesture swipe:3:right workspace prev
      bindgesture swipe:3:left workspace next

      exec gsettings set org.gnome.desktop.interface color-scheme prefer-dark
    '';
    wrapperFeatures = {
      gtk = true;
    };
    systemd.enable = true;
    xwayland = true;
  };

  gtk = {
    enable = true;
    font = {
      name = "sans-serif";
      size=12.0;
    };
    iconTheme = {
      package = pkgs.yaru-theme;
      name = "Yaru-blue-dark";
    };
    theme = {
      package = pkgs.yaru-theme;
      name = "Yaru-blue-dark";
    };
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
  };

  services.mako = {
    enable = true;
    anchor = "top-center";
    font = "sans-serif 12";
    defaultTimeout = 5000;
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    client.enable = true;
    defaultEditor = true;
    startWithUserSession = "graphical";
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
      local wezterm = require'wezterm'

      return {
          enable_wayland = false,
          colors = {
              foreground = '#FFFFFF',
              background = 'rgba(18, 18, 18, 0.8)',
          },
          font = wezterm.font_with_fallback({ "IosevkaTermSS07 Nerd Font", "Noto Color Emoji" }),
          font_size = 15.5,
          hide_tab_bar_if_only_one_tab = true,
      }
    '';
  };

  # programs.opam.enable = true;

  programs.obs-studio.enable = true;
  
  programs.home-manager.enable = true;
}
