;;; package -- Init  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Performance tweaks for modern machines
;; max memory available for gc on startup
(defvar me/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun me/defer-garbage-collection-h ()
  "Max memory available for gc when opening minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)
(setq garbage-collection-messages t)

(setq read-process-output-max (* 1024 1024)) ; 1mb
(defvar comp-deferred-compliation)

(setq package-enable-at-startup nil)

(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))


;; Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)


(use-package system-packages
  :straight t)
(use-package use-package-ensure-system-package
  :straight t)
(use-package auto-package-update
  :straight t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  )


(use-package which-key
  :straight t
  :config
  (which-key-mode))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; highlight the current line
(global-hl-line-mode t)

(setq-default cursor-type 'bar)

;; always highlight code
(global-font-lock-mode 1)
;; refresh a buffer if changed on disk
(global-auto-revert-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)



(setq default-directory "~/"
      ;; always follow symlinks when opening files
      vc-follow-symlinks t
      ;; quiet startup
      inhibit-startup-message t
      initial-scratch-message nil
      ;; hopefully all themes we install are safe
      custom-safe-themes t
      ;; simple lock/backup file management
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      ;; when quiting emacs, just kill processes
      confirm-kill-processes nil
      ;; ask if local variables are safe once.
      enable-local-variables t
      ;; life is too short to type yes or no
      use-short-answers t
      backup-directory-alist '((".*" . ,temporary-file-directory))
      )


;; use human-readable sizes in dired
(setq-default dired-listing-switches "-alh")

;; Remove extra UI clutter by hiding the scrollbar, menubar, and toolbar.
(menu-bar-mode -1)
(tool-bar-mode -1)
;;(scroll-bar-mode -1)

;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)
;; Always show matching paranthesis, line and column number
(custom-set-variables '(show-paren-mode 1)
		      '(line-number-mode t)
		      '(column-number-mode t)
		      '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Automatically save your place in files
(save-place-mode t)

;; Save history in minibuffer to keep recent commands easily accessible
(savehist-mode t)

;; Keep track of open files
(recentf-mode t)

(global-set-key (kbd "C-x <up>") #'other-window)
(global-set-key (kbd "C-x <down>") #'previous-window-any-frame)


;; Keep files up-to-date when they change outside Emacs
(global-auto-revert-mode t)

;; Display line numbers only when in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
  :after vertico
  :straight t
  :init
  (marginalia-mode))

;; Adds intellisense-style code completion at point that works great
;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :straight t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package magit
  :straight t
  :bind (("C-c g" . magit-status))
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t)
  )

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq ns-right-alternate-modifier 'none)

  (setq dashboard-items '((recents . 5)
 			  (projects . 5)))
  )

;; Projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package projectile-ripgrep
  :straight t
  :after projectile)


(use-package yasnippet
  :straight t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))


;; Extended completion utilities
(use-package consult
  :straight t
  :config
  (global-set-key [rebind switch-to-buffer] #'consult-buffer)
  (global-set-key (kbd "C-c j") #'consult-line)
  (global-set-key (kbd "C-c i") #'consult-imenu)
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)

  )


;; Flycheck
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))



;; Miscellaneous options
(setq-default major-mode8
              (lambda ()             ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;(setq window-resize-pixelwise t)
;;(setq frame-resize-pixelwise t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;===============
;; UI
;;===============
(set-frame-font "Source Code Pro 12")
(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-kill-emacs #'yes-or-no-p)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile)
  )

;; Treemacs
(use-package treemacs
  :straight t
  :defer t
  :commands (treemacs)
  :after (lsp-mode)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package all-the-icons
  :straight t)
(use-package all-the-icons-dired
  :straight t)

(use-package treemacs-all-the-icons
  :straight t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-solarized-light t)
  (load-theme 'doom-solarized-dark t)

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  )

(if (display-graphic-p)
    (enable-theme 'doom-solarized-light)
  (enable-theme 'doom-solarized-dark))


(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

;; Disable splash screen
(setq inhibit-startup-screen t)

;;================
;; Programming
;;================
(use-package diminish
  :straight (:host github :repo "emacsmirror/diminish" :files ("*.el"))
  :straight t)

;; Automatically guess indent offsets, tab, spaces settings, etc.
(use-package dtrt-indent
  :straight t)

;; ;; Enable autocompletion by default in programming buffers
;;(add-hook 'prog-mode-hook #'corfu-mode)


;;; Indication of local VCS changes
(use-package diff-hl
  :straight t
  :config
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  )

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

(use-package diff-mode
  :straight t
  :mode
  "\\.patch[0-9]*\\'"
  )

;; EditorConfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  )
(use-package editorconfig-generate
  :straight t
  )

(use-package editorconfig-domain-specific
  :straight t
  )

(use-package editorconfig-custom-majormode
  :straight t)

;; CI and build languages
(use-package gitlab-ci-mode
  :straight t
  )

(use-package make-mode
  :straight nil
  :init
  (add-to-list 'auto-mode-alist '("Makefile$" . makefile-mode))
  (add-to-list 'auto-mode-alist '("makefile$" . makefile-mode))
  :mode ("Makefile\\'" "makefile\\'" "Make.obj\\'"))

(use-package cmake-mode
  :straight t
  :mode
  ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )

(use-package dockerfile-mode
  :straight t
  )


(use-package docker-compose-mode
  :straight t
  )
(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

;; Config and markup languages
(use-package hcl-mode
  :straight t
  )


;;; JSON Support
(use-package json-snatcher
  :straight t
  )
(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :after
  (json-snatcher)
  )

;;; Markdown support
(use-package markdown-mode
  :straight t)

(use-package markdown-toc
  :straight t
  :after markdown-mode
  )


;; Languages
;;; Haskell Support
(use-package haskell-mode
  :straight t)

(use-package go-mode
  :straight t
  :after
  (lsp-mode)
  :bind (:map go-mode-map
 	      ("C-c C-f" . 'gofmt))
  ;:hook (before-save . gofmt-before-save)
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)
  )

(use-package yaml-mode
  :straight t)

;; Python
(use-package highlight-indentation
  :straight t)
(use-package python
  :straight nil
  :after
  (highlight-indentation)
  :init
  (progn
    (add-hook 'python-mode-hook 'highlight-indentation-mode)
    ;; (add-hook 'python-mode-hook 'eldoc-mode)
    (add-hook 'python-mode-hook 'sphinx-doc-mode))
  :bind (:map python-mode-map
	      (
               ("C-c i" . python-insert-docstring-with-google-style-at-point)
               ))
  :config

  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  )
(use-package python-insert-docstring
  :straight t)
(use-package blacken
  :straight t)

(use-package python-black
  :straight t)

(use-package lsp-pyright
  :straight t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  
  :config
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off")
  (lsp-pyright-python-executable-cmd "python3")

  )

(use-package pyvenv
  :straight t
  :after python
  :config
  (defun dn-activate-pyvenv ()
    "Activate python environment according to the `.venv' file."
    (interactive)
    (pyvenv-mode)
    (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
      (if (file-exists-p pfile)
          (pyvenv-workon (with-temp-buffer
                           (insert-file-contents pfile)
                           (nth 0 (split-string (buffer-string))))))))
  )

(use-package cython-mode
  :straight t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))

(use-package tree-sitter
  :straight t
  :config

)
(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

  (global-tree-sitter-mode)


;; lsp-mode
(use-package lsp-mode
  :straight t
  :defines lsp-language-id-configuration
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
	 (go-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (haskell-mode . lsp)
	 (asm-mode . lsp)
	 (shell-script-mode . lsp)
	 (cmake-mode . lsp)
	 (dockerfile-mode . lsp)
	 (html-mode . lsp)
	 (javascript-mode . lsp)
	 (typescript-ts-mode . lsp)
	 (json-mode . lsp)
	 (markdown-mode . lsp)
	 (nix-mode . lsp)
	 (semgrep-mode . lsp)
	 (terraform-mode . lsp)
	 )
  :commands lsp
  :custom
  (lsp-use-plists t)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 3 1024 1024))

  ;; (treemacs-space-between-root-nodes nil)

  (lsp-auto-guess-root t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-before-save-edits nil)
  (lsp-idle-delay 0.3)
  (lsp-completion-provider :capf)
  ;; Prevent constant auto-formatting...)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  ;; be more ide-ish)
  (lsp-headerline-breadcrumb-enable t)
  ;; python-related settings)
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  )
;; Taken from https://tychoish.com/post/emacs-and-lsp-mode/
(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-alignment 'at-point)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-childframe t)
  ;; (lsp-ui-doc-use-webkit nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode 'line)
  ;; :custom-face
  ;; (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :config
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))
  )

;; Debug
(use-package dap-mode
  :straight t
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :after (lsp-mode)
  :functions dap-hydra/nil
  :custom
  (dap-auto-configure-mode t)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions controls tooltip))
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (python-mode . (lambda () (require 'dap-python)))
         (diff-mode . (lambda () (dap-mode -1)))
         (powershell-mode . (lambda () (dap-mode -1)))
         (shell-script-mode . (lambda () (dap-mode -1)))
         (cmake-mode . (lambda () (dap-mode -1)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :init
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  )
(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1)
  ;; :bind (:map lsp-mode-map
  ;;        ("M-9" . lsp-treemacs-errors-list))
  )

;; Company
(use-package company
  :straight t
  :config
  ;; No delay in showing suggestions.
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-tooltip-annotation-padding 1
      company-tooltip-flip-when-above t
      company-text-face-extra-attributes
      '(:weight bold :slant italic)
      )
      (add-hook 'after-init-hook 'global-company-mode)
)




(use-package lsp-haskell
  :straight t
  :after lsp-mode
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

(provide 'init)
;;; init.el ends here
