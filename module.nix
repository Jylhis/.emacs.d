{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs;

  # Developer tools packages organized by language
  developerToolsPackages =
    let
      langs = cfg.developerTools.languages;
    in
    lib.optionals cfg.developerTools.enable (
      (lib.optionals langs.python [
        pkgs.pyright
        pkgs.ruff
        pkgs.black
      ])
      ++ (lib.optionals langs.rust [
        pkgs.rust-analyzer
        pkgs.rustfmt
        pkgs.clippy
      ])
      ++ (lib.optionals langs.go [
        pkgs.gopls
        pkgs.gofumpt
        pkgs.golangci-lint
      ])
      ++ (lib.optionals langs.typescript [
        pkgs.nodePackages.typescript-language-server
        pkgs.nodePackages.prettier
        pkgs.nodePackages.eslint
      ])
      ++ (lib.optionals langs.nix [
        pkgs.nil
        pkgs.nixfmt-rfc-style
        pkgs.statix
        pkgs.deadnix
      ])
      ++ (lib.optionals langs.c-cpp [
        pkgs.clang-tools # includes clangd, clang-format, clang-tidy
      ])
      ++ (lib.optionals langs.bash [
        pkgs.nodePackages.bash-language-server
        pkgs.shellcheck
        pkgs.shfmt
      ])
      ++ (lib.optionals langs.yaml [ pkgs.yaml-language-server ])
      ++ (lib.optionals langs.json [ pkgs.nodePackages.vscode-langservers-extracted ])
      ++ (lib.optionals langs.markdown [ pkgs.marksman ])
      ++ cfg.developerTools.extraPackages
    );
in
{
  options = {
    programs.emacs.userConfig = lib.mkOption {
      type = lib.types.path;
      default = ./.; # TODO: use config package
    };

    programs.emacs.developerTools = {
      enable = lib.mkEnableOption "developer tools for IDE-like experience";

      languages = {
        python = lib.mkEnableOption "Python tools (pyright, ruff, black)";
        rust = lib.mkEnableOption "Rust tools (rust-analyzer, rustfmt, clippy)";
        go = lib.mkEnableOption "Go tools (gopls, gofumpt, golangci-lint)";
        typescript = lib.mkEnableOption "TypeScript/JavaScript tools (typescript-language-server, prettier, eslint)";
        nix = lib.mkEnableOption "Nix tools (nil, nixfmt, statix, deadnix)";
        c-cpp = lib.mkEnableOption "C/C++ tools (clangd, clang-format, clang-tidy)";
        bash = lib.mkEnableOption "Bash tools (bash-language-server, shellcheck, shfmt)";
        yaml = lib.mkEnableOption "YAML tools (yaml-language-server)";
        json = lib.mkEnableOption "JSON tools (vscode-langservers-extracted)";
        markdown = lib.mkEnableOption "Markdown tools (marksman)";
      };

      extraPackages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
        description = "Additional developer tool packages to install";
        example = lib.literalExpression "[ pkgs.jq pkgs.yq ]";
      };
    };
  };
  config = lib.mkIf config.programs.emacs.enable {
    home = {
      file = {
        # Dynamically install all Emacs configuration files using filesets
        ".config/emacs" = {
          source = cfg.userConfig;
          recursive = true;
        };
      };
      shellAliases = {
        emc = "emacsclient -t -a emacs";
        emcg = "emacsclient -c -a emacs";
        emqg = "emacs -nw -Q";
        emq = "emacs -Q";
      };
      # TODO: move to fonts.nix
      packages =
        with pkgs.nerd-fonts;
        [
          dejavu-sans-mono
          envy-code-r
          fira-code
          fira-mono
          go-mono
          hack
          hasklug
          im-writing
          monaspace
          symbols-only
        ]
        ++ (with pkgs; [
          jetbrains-mono
          inter
          source-code-pro
        ])
        ++ developerToolsPackages;
    };

    services = {
      emacs = {
        enable = lib.mkDefault true;
        defaultEditor = true;

        client = {
          enable = lib.mkDefault true;
          arguments = [
            "-c"
            "-a"
            "emacs"
          ];
        };
        socketActivation.enable = true;
      };
    };
  };
}
