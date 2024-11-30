{pkgs, ...}: {
  home.username = "qak";
  home.homeDirectory = "/home/qak";
  home.stateVersion = "23.11";
  home.packages = with pkgs; [
    # Apps (among other things)
    audacity
    btop
    ffmpeg
    discord
    evince
    glaxnimate
    gnome.gnome-characters
    gnome.gnome-system-monitor
    imv
    kdenlive
    libreoffice-qt6-fresh
    libxml2
    kdePackages.mlt
    obsidian
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
    noto-fonts-cjk
    noto-fonts-color-emoji
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
    # Games
    temurin-bin-17
    prismlauncher
  ];

  home.file = {
    ".config/emacs/codemacs/early-init.el".source = ./minmacs/codemacs/early-init.el;
    ".config/emacs/codemacs/init.el".source = ./minmacs/codemacs/init.el;
    ".config/emacs/mathmacs/early-init.el".source = ./minmacs/mathmacs/early-init.el;
    ".config/emacs/mathmacs/init.el".source = ./minmacs/mathmacs/init.el;
    ".config/emacs/common".source = ./minmacs/common;
    ".config/electron-flags.conf".source = ./electron-flags.conf;
    ".config/starship.toml".source = ./starship.toml;
    ".local/share/applications/discord.desktop".source = ./discord.desktop;
    ".local/share/applications/codemacs.desktop".source = ./codemacs.desktop;
    ".local/share/applications/mathmacs.desktop".source = ./mathmacs.desktop;
    ".local/share/icons/discord.png".source = ./discord.png;
  };

  home.sessionVariables = {};

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

  # xdg = {
  #   mime.enable = true;
  #   mimeApps = {
  #     enable = true;
  #     defaultApplications = {
  #       "inode/directory" = "nemo.desktop";
  #       "x-scheme-handler/https" = "firefox.desktop";
  #       "x-scheme-handler/http" = "firefox.desktop";
  #       "text/html" = "firefox.desktop";
  #       "image/gif" = "imv.desktop";
  #       "image/jpeg" = "imv.desktop";
  #       "image/png" = "imv.desktop";
  #       "application/ogg" = "org.gnome.Rhythmbox3.desktop";
  #       "audio/x-mp3" = "org.gnome.Rhythmbox3.desktop";
  #       "video/avi" = "vlc.desktop";
  #       "video/mp4" = "vlc.desktop";
  #       "video/webm" = "vlc.desktop";
  #       "text/plain" = "emacs.desktop";
  #       "application/x-shellscript" = "emacs.desktop";
  #       "application/pdf" = "org.gnome.Evince.desktop";
  #       "image/tiff" = "org.gnome.Evince.desktop";
  #       "application/postscript" = "org.gnome.Evince.desktop";
  #       "application/x-dvi" = "org.gnome.Evince.desktop";
  #     };
  #   };
  #   userDirs.createDirectories.enable = true;
  # };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
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
          font_size = 15.5,
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
