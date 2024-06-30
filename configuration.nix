{ config, lib, pkgs, ... }:

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

  environment.systemPackages = with pkgs; [
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
  ];

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
    promptInit = ''
      function prompt_login --description 'display user name for the prompt'
        if not set -q __fish_machine
            set -g __fish_machine
            set -l debian_chroot $debian_chroot

            if test -r /etc/debian_chroot
                set debian_chroot (cat /etc/debian_chroot)
            end

            if set -q debian_chroot[1]
                and test -n "$debian_chroot"
                set -g __fish_machine "(chroot:$debian_chroot)"
            end
        end

        # Prepend the chroot environment if present
        if set -q __fish_machine[1]
            echo -n -s (set_color yellow) "$__fish_machine" (set_color normal) ' '
        end

        # If we're running via SSH, change the host color.
        set -l color_host $fish_color_host
        if set -q SSH_TTY; and set -q fish_color_host_remote
            set color_host $fish_color_host_remote
        end
        set -l fish_color_user cyan --bold
        set -l color_host brblue --italics

        echo -n -s (set_color $fish_color_user) "$USER" (set_color grey) ∈ (set_color $color_host) (prompt_hostname) (set_color normal)
    end

    function fish_prompt --description 'Write out the prompt'
        set -l last_pipestatus $pipestatus
        set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
        set -l normal (set_color normal)
        set -q fish_color_status
        or set -g fish_color_status red

        # Color the prompt differently when we're root
        set -l color_cwd grey --italics
        set -l suffix ' λx.'
        if functions -q fish_is_root_user; and fish_is_root_user
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            end
            set suffix '#'
        end

        # Write pipestatus
        # If the status was carried over (if no command is issued or if `set` leaves the status untouched), don't bold it.
        set -l bold_flag --bold
        set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
        if test $__fish_prompt_status_generation = $status_generation
            set bold_flag
        end
        set __fish_prompt_status_generation $status_generation
        set -l status_color (set_color $fish_color_status)
        set -l statusb_color (set_color $bold_flag $fish_color_status)
        set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

        echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status (set_color bryellow --bold) $suffix " "
    end
    '';
  };
  
  programs.sway = {
    enable = true;
    package = null;
    wrapperFeatures.base = false;
    extraPackages = [];
    extraOptions = [];
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

