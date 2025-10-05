;;; test-ui.el --- Test UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for UI and appearance configuration including themes, icons, and visual enhancements.

;;; Code:

(require 'test-helper)
(require 'ui)

;;; Theme Configuration

(ert-deftest test-custom-themes-trusted ()
  "Verify all themes are trusted by default."
  :tags '(unit fast ui)
  (should (eq custom-safe-themes t)))

(ert-deftest test-modus-theme-loaded ()
  "Verify a modus theme is loaded."
  :tags '(unit fast ui)
  (should (or (custom-theme-enabled-p 'modus-vivendi-tinted)
              (custom-theme-enabled-p 'modus-operandi-tinted))))

(ert-deftest test-modus-theme-toggle-binding ()
  "Verify modus theme toggle keybinding."
  :tags '(unit fast ui keybindings)
  (should (eq (key-binding (kbd "C-c t")) 'modus-themes-toggle)))

(ert-deftest test-modus-themes-configuration ()
  "Verify modus themes configuration."
  :tags '(unit fast ui)
  (require 'modus-themes)
  (should (eq modus-themes-italic-constructs t))
  (should (eq modus-themes-bold-constructs nil))
  (should (eq modus-themes-mixed-fonts t))
  (should (eq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))))

;;; Auto Dark Mode

(ert-deftest test-auto-dark-mode ()
  "Verify auto-dark mode is configured."
  :tags '(unit ui)
  (when (test-helper-package-available-p 'auto-dark)
    (require 'auto-dark)
    (should (test-helper-mode-active-p 'auto-dark-mode))))

;;; GUI Elements

(ert-deftest test-tool-bar-disabled ()
  "Verify tool-bar is disabled."
  :tags '(unit fast ui)
  (should (not tool-bar-mode)))

(ert-deftest test-scroll-bar-disabled ()
  "Verify scroll-bar is disabled."
  :tags '(unit fast ui)
  (should (not scroll-bar-mode)))

;;; Enhanced UI Modes

(ert-deftest test-which-key-mode ()
  "Verify which-key mode is active."
  :tags '(unit fast ui)
  (should (member 'which-key-mode after-init-hook)))

(ert-deftest test-global-hl-line-mode ()
  "Verify global-hl-line-mode is active."
  :tags '(unit fast ui)
  (should (member 'global-hl-line-mode after-init-hook)))

(ert-deftest test-winner-mode ()
  "Verify winner-mode is active."
  :tags '(unit fast ui)
  (require 'winner)
  (should (test-helper-mode-active-p 'winner-mode)))

(ert-deftest test-winner-keybindings ()
  "Verify winner mode keybindings."
  :tags '(unit fast ui keybindings)
  (should (eq (key-binding (kbd "C-c u")) 'winner-undo))
  (should (eq (key-binding (kbd "C-c r")) 'winner-redo)))

(ert-deftest test-show-paren-mode ()
  "Verify show-paren-mode is active."
  :tags '(unit fast ui)
  (should (member 'show-paren-mode after-init-hook)))

(ert-deftest test-show-paren-configuration ()
  "Verify show-paren mode configuration."
  :tags '(unit fast ui)
  (require 'paren)
  (should (eq show-paren-delay 0.1))
  (should (eq show-paren-highlight-openparen t))
  (should (eq show-paren-when-point-inside-paren t))
  (should (eq show-paren-when-point-in-periphery t)))

(ert-deftest test-pixel-scroll-precision ()
  "Verify pixel-scroll-precision-mode when in GUI."
  :tags '(unit ui)
  (when (display-graphic-p)
    (should (test-helper-mode-active-p 'pixel-scroll-precision-mode))))

;;; Icons and Visual Enhancements

(ert-deftest test-nerd-icons-available ()
  "Verify nerd-icons package is available."
  :tags '(unit fast ui)
  (should (test-helper-package-available-p 'nerd-icons)))

(ert-deftest test-nerd-icons-corfu ()
  "Verify nerd-icons-corfu integration."
  :tags '(unit ui)
  (when (and (test-helper-package-available-p 'nerd-icons)
             (test-helper-package-available-p 'corfu))
    (should (member 'nerd-icons-corfu-formatter corfu-margin-formatters))))

(ert-deftest test-nerd-icons-completion ()
  "Verify nerd-icons-completion mode."
  :tags '(unit ui)
  (when (test-helper-package-available-p 'nerd-icons-completion)
    (require 'nerd-icons-completion)
    (should (test-helper-mode-active-p 'nerd-icons-completion-mode))))

(ert-deftest test-breadcrumb-mode-hook ()
  "Verify breadcrumb mode is hooked to prog-mode."
  :tags '(unit ui)
  (when (test-helper-package-available-p 'breadcrumb)
    (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
      (should (test-helper-mode-active-p 'breadcrumb-mode)))))

(ert-deftest test-rainbow-delimiters-hook ()
  "Verify rainbow-delimiters in lisp modes."
  :tags '(unit ui)
  (when (test-helper-package-available-p 'rainbow-delimiters)
    (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
      (should (test-helper-mode-active-p 'rainbow-delimiters-mode)))))

;;; hl-todo Configuration

(ert-deftest test-hl-todo-in-prog-mode ()
  "Verify hl-todo mode activates in prog-mode."
  :tags '(unit ui)
  (when (test-helper-package-available-p 'hl-todo)
    (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
      (should (test-helper-mode-active-p 'hl-todo-mode)))))

(ert-deftest test-hl-todo-keywords ()
  "Verify hl-todo keywords are configured."
  :tags '(unit fast ui)
  (when (boundp 'hl-todo-keyword-faces)
    (should (assoc "TODO" hl-todo-keyword-faces))
    (should (assoc "FIXME" hl-todo-keyword-faces))
    (should (assoc "HACK" hl-todo-keyword-faces))))

;;; Calendar Configuration

(ert-deftest test-calendar-configuration ()
  "Verify calendar configuration."
  :tags '(unit fast ui)
  (should (eq calendar-week-start-day 1))) ; Monday

(provide 'test-ui)
;;; test-ui.el ends here
