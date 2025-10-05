{
  pkgs ? import <nixpkgs> { config.allowUnfree = true; },
  emacs ? pkgs.emacs,
}:
let
  emacsWithPackages = emacs.pkgs.withPackages (
    epkgs: with epkgs; [
      # TODO: Package: https://github.com/flymake/flymake-elsa
      adoc-mode
      modus-themes
      sideline
      sideline-flymake
      sideline-eglot
      ansible
      auth-source-1password
      auto-dark
      avy
      awk-ts-mode
      bitbake-ts-mode
      breadcrumb
      cape
      cmake-mode
      compile-multi
      compile-multi-embark
      compile-multi-nerd-icons
      consult
      consult-compile-multi
      consult-dash
      consult-eglot
      consult-eglot-embark
      consult-flyspell
      corfu
      csv-mode
      cuda-mode
      dape
      dash-docs
      demangle-mode
      devicetree-ts-mode
      diff-hl
      diminish
      direnv
      docker-compose-mode
      dockerfile-mode
      drag-stuff
      dtrt-indent
      editorconfig
      elisp-lint
      elsa
      embark
      embark-consult
      emojify
      expand-region
      projection
      projection-multi-embark
      projection-multi
      projection-dape
      git-commit-ts-mode
      gitlab-ci-mode
      gnuplot
      go-mode
      haskell-mode
      haskell-ts-mode
      helpful
      hl-todo
      jq-ts-mode
      just-mode
      just-ts-mode
      ligature
      logview
      magit
      magit-todos
      marginalia
      markdown-mode
      mermaid-mode
      mermaid-ts-mode
      modern-cpp-font-lock
      multiple-cursors
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      nerd-icons-ibuffer
      nix-mode
      nix-ts-mode
      # obsidian
      orderless
      org-appear
      org-jira
      org-modern
      ox-gfm
      ox-hugo
      ox-jira
      ox-slack
      package-lint
      pkgs.sqlite # consult-dash dependency
      pretty-sha-path
      rainbow-delimiters
      smartparens
      sops
      sql-indent
      ssh-config-mode
      super-save
      terraform-mode
      tree-sitter-langs
      treesit-auto
      treesit-fold
      treesit-grammars.with-all-grammars
      vertico
      vterm
      vundo
      web-mode
      web-server # For claude-code-ide
      websocket # For claude-code-ide
      wgrep
      yaml-mode
      zoxide
    ]
  );
in
emacsWithPackages.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or { }) // {
    tests = pkgs.runCommand "emacs-config-tests" { } ''
      echo "Running Emacs configuration tests..."
      ${emacsWithPackages}/bin/emacs -Q --batch \
        --eval "(progn \
                  (add-to-list 'load-path \"${./.}\") \
                  (add-to-list 'load-path \"${./.}/lisp\") \
                  (add-to-list 'load-path \"${./.}/tests\") \
                  (add-to-list 'load-path \"${./.}/config\"))" \
        --eval "(require 'ert)" \
        --eval "(require 'cl-lib)" \
        --load "${./.}/tests/test-helper.el" \
        --load "${./.}/tests/test-config-loading.el" \
        --load "${./.}/tests/test-core.el" \
        --load "${./.}/tests/test-fonts.el" \
        --load "${./.}/tests/test-ui.el" \
        --load "${./.}/tests/test-completion.el" \
        --load "${./.}/tests/test-programming.el" \
        --load "${./.}/tests/test-git.el" \
        --load "${./.}/tests/test-writing.el" \
        --load "${./.}/tests/test-help.el" \
        --load "${./.}/tests/test-per-project.el" \
        --load "${./.}/tests/test-keybindings.el" \
        --load "${./.}/tests/test-android.el" \
        --load "${./.}/tests/test-utils.el" \
        --load "${./.}/tests/test-platform.el" \
        --load "${./.}/tests/test-auth-source-1password.el" \
        --load "${./.}/tests/integration/test-completion-workflow.el" \
        --load "${./.}/tests/integration/test-lsp-workflow.el" \
        --load "${./.}/tests/integration/test-git-workflow.el" \
        --eval "(ert-run-tests-batch-and-exit)"
      echo "All tests passed!"
      touch $out
    '';
  };
})
