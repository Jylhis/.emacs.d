;;; test-programming.el --- Test programming configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for programming and development configuration including LSP, tree-sitter,
;; flymake, and major modes.

;;; Code:

(require 'test-helper)
(require 'programming)

;;; Basic Programming Modes

(ert-deftest test-line-numbers-in-prog-mode ()
  "Verify line numbers display in prog-mode."
  :tags '(unit programming)
  (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
    (should display-line-numbers-mode)))

;;; Tree-sitter

(ert-deftest test-treesit-configuration ()
  "Verify tree-sitter configuration."
  :tags '(unit fast programming)
  (should (eq treesit-font-lock-level 4)))

(ert-deftest test-global-treesit-auto-mode ()
  "Verify global-treesit-auto-mode is active."
  :tags '(unit programming)
  (when (test-helper-package-available-p 'treesit-auto)
    (should (test-helper-mode-active-p 'global-treesit-auto-mode))))

;;; Flymake

(ert-deftest test-flymake-configuration ()
  "Verify flymake configuration."
  :tags '(unit fast programming)
  (should (eq flymake-fringe-indicator-position 'left-fringe))
  (should (eq flymake-suppress-zero-counters t)))

(ert-deftest test-flymake-keybindings ()
  "Verify flymake keybindings."
  :tags '(unit programming keybindings)
  (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
    (when flymake-mode
      (let ((map flymake-mode-map))
        (should (keymapp map))
        (should (eq (lookup-key map (kbd "M-n")) 'flymake-goto-next-error))
        (should (eq (lookup-key map (kbd "M-p")) 'flymake-goto-prev-error))))))

(ert-deftest test-flymake-diagnostic-functions ()
  "Verify flymake diagnostic helper functions."
  :tags '(unit fast programming)
  (should (fboundp 'j10s/flymake-show-diagnostic-at-point))
  (should (fboundp 'j10s/flymake-show-diagnostic-delayed)))

;;; Flyspell

(ert-deftest test-flyspell-in-prog-mode ()
  "Verify flyspell-prog-mode hook."
  :tags '(unit programming)
  (should (member 'flyspell-prog-mode prog-mode-hook)))

(ert-deftest test-flyspell-in-text-mode ()
  "Verify flyspell-mode hook for text."
  :tags '(unit programming)
  (should (member 'flyspell-mode text-mode-hook)))

;;; Eglot (LSP)

(ert-deftest test-eglot-configuration ()
  "Verify eglot basic configuration."
  :tags '(unit fast programming)
  (should (eq eglot-send-changes-idle-time 0.5))
  (should (eq eglot-autoshutdown t))
  (should (eq eglot-events-buffer-size 0))
  (should (eq eglot-report-progress nil))
  (should (eq eglot-extend-to-xref t)))

(ert-deftest test-eglot-hooks ()
  "Verify eglot activates for appropriate modes."
  :tags '(unit programming)
  ;; Check that eglot-ensure is in prog-mode-hook with exclusions
  (should (member 'prog-mode-hook (mapcar #'car hook-list))))

(ert-deftest test-eglot-keybindings ()
  "Verify eglot keybindings."
  :tags '(unit programming keybindings)
  (let ((map eglot-mode-map))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "C-c r r")) 'eglot-rename))
    (should (eq (lookup-key map (kbd "C-c r f")) 'eglot-format))
    (should (eq (lookup-key map (kbd "C-c r a")) 'eglot-code-actions))
    (should (eq (lookup-key map (kbd "C-h .")) 'eldoc-doc-buffer))))

(ert-deftest test-eglot-server-programs ()
  "Verify eglot server programs configured."
  :tags '(unit fast programming)
  (should (assoc '(go-mode go-ts-mode) eglot-server-programs)))

(ert-deftest test-consult-eglot-available ()
  "Verify consult-eglot integration."
  :tags '(unit programming)
  (when (test-helper-package-available-p 'consult-eglot)
    (let ((map eglot-mode-map))
      (should (eq (lookup-key map (kbd "C-M-.")) 'consult-eglot-symbols)))))

;;; Xref

(ert-deftest test-xref-ripgrep ()
  "Verify xref uses ripgrep when available."
  :tags '(unit programming)
  (when (executable-find "rg")
    (should (eq xref-search-program 'ripgrep))))

;;; Direnv

(ert-deftest test-direnv-mode ()
  "Verify direnv-mode when direnv is available."
  :tags '(unit programming)
  (when (executable-find "direnv")
    (should (test-helper-mode-active-p 'direnv-mode))))

;;; Debugging

(ert-deftest test-gdb-configuration ()
  "Verify gdb configuration."
  :tags '(unit fast programming)
  (should (eq gdb-many-windows t))
  (should (eq gdb-show-main t)))

(ert-deftest test-dape-configuration ()
  "Verify dape configuration."
  :tags '(unit programming)
  (when (test-helper-package-available-p 'dape)
    (should (eq dape-buffer-window-arrangement 'gud))
    (should (eq dape-inlay-hints t))))

;;; Wgrep

(ert-deftest test-wgrep-configuration ()
  "Verify wgrep configuration."
  :tags '(unit fast programming)
  (should (eq wgrep-auto-save-buffer t))
  (should (eq wgrep-change-readonly-file t)))

;;; Major Modes

(ert-deftest test-markdown-mode ()
  "Verify markdown-mode configuration."
  :tags '(unit programming)
  (should (eq (cdr (assoc "\\.md\\'" auto-mode-alist)) 'markdown-mode))
  (should (eq (cdr (assoc "README\\.md\\'" auto-mode-alist)) 'gfm-mode)))

(ert-deftest test-nix-mode ()
  "Verify nix modes are available."
  :tags '(unit fast programming)
  (should (test-helper-package-available-p 'nix-mode))
  (should (test-helper-package-available-p 'nix-ts-mode)))

(ert-deftest test-nix-ts-mode-association ()
  "Verify nix-ts-mode is associated with .nix files."
  :tags '(unit fast programming)
  (should (eq (cdr (assoc "\\.nix\\'" auto-mode-alist)) 'nix-ts-mode)))

(ert-deftest test-go-mode-available ()
  "Verify go-mode is available."
  :tags '(unit fast programming)
  (should (test-helper-package-available-p 'go-mode)))

(ert-deftest test-yaml-mode-available ()
  "Verify yaml-mode is available."
  :tags '(unit fast programming)
  (should (test-helper-package-available-p 'yaml-mode)))

(ert-deftest test-web-mode ()
  "Verify web-mode configuration."
  :tags '(unit programming)
  (when (test-helper-package-available-p 'web-mode)
    (should (eq web-mode-markup-indent-offset 2))
    (should (eq web-mode-css-indent-offset 2))
    (should (eq web-mode-code-indent-offset 2))))

(ert-deftest test-c++-mode-association ()
  "Verify C++ file associations."
  :tags '(unit fast programming)
  (should (eq (cdr (assoc "\\.h\\'" auto-mode-alist)) 'c++-mode))
  (should (eq (cdr (assoc "\\.hpp\\'" auto-mode-alist)) 'c++-mode))
  (should (eq (cdr (assoc "\\.cpp\\'" auto-mode-alist)) 'c++-mode)))

(ert-deftest test-just-mode-available ()
  "Verify just-mode is available."
  :tags '(unit fast programming)
  (should (test-helper-package-available-p 'just-mode)))

(ert-deftest test-cmake-mode-available ()
  "Verify cmake-mode is available."
  :tags '(unit fast programming)
  (should (test-helper-package-available-p 'cmake-mode)))

;;; EditorConfig

(ert-deftest test-editorconfig-mode ()
  "Verify editorconfig-mode is active."
  :tags '(unit programming)
  (should (test-helper-mode-active-p 'editorconfig-mode)))

;;; Eldoc

(ert-deftest test-eldoc-configuration ()
  "Verify eldoc configuration."
  :tags '(unit fast programming)
  (should (eq eldoc-idle-delay 0.5))
  (should (eq eldoc-print-after-edit t)))

;;; Terminal Mouse

(ert-deftest test-xterm-mouse ()
  "Verify xterm mouse mode."
  :tags '(unit programming)
  (should (eq xterm-mouse-mode 1)))

(provide 'test-programming)
;;; test-programming.el ends here
