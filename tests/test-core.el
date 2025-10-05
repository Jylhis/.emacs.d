;;; test-core.el --- Test core configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for core Emacs configuration including built-in packages and essential settings.

;;; Code:

(require 'test-helper)
(require 'core)

;;; Basic Emacs Settings

(ert-deftest test-use-short-answers ()
  "Verify short answers are enabled."
  :tags '(unit fast core)
  (should (eq use-short-answers t)))

(ert-deftest test-lockfiles-disabled ()
  "Verify lockfiles are disabled."
  :tags '(unit fast core)
  (should (eq create-lockfiles nil)))

(ert-deftest test-delete-to-trash ()
  "Verify delete by moving to trash is enabled."
  :tags '(unit fast core)
  (should (eq delete-by-moving-to-trash t)))

(ert-deftest test-sentence-spacing ()
  "Verify modern sentence spacing."
  :tags '(unit fast core)
  (should (eq sentence-end-double-space nil)))

(ert-deftest test-completion-case-insensitive ()
  "Verify case-insensitive completion."
  :tags '(unit fast core)
  (should (eq completion-ignore-case t))
  (should (eq read-buffer-completion-ignore-case t))
  (should (eq read-file-name-completion-ignore-case t)))

(ert-deftest test-load-prefer-newer ()
  "Verify loading prefers newer files."
  :tags '(unit fast core)
  (should (eq load-prefer-newer t)))

;;; Core Keybindings

(ert-deftest test-c-z-unbound ()
  "Verify C-z is unbound (no suspend-frame)."
  :tags '(unit fast core keybindings)
  (should (null (key-binding (kbd "C-z")))))

(ert-deftest test-c-x-c-z-unbound ()
  "Verify C-x C-z is unbound."
  :tags '(unit fast core keybindings)
  (should (null (key-binding (kbd "C-x C-z")))))

(ert-deftest test-m-o-other-window ()
  "Verify M-o is bound to other-window."
  :tags '(unit fast core keybindings)
  (should (eq (key-binding (kbd "M-o")) 'other-window)))

(ert-deftest test-m-p-scroll-down ()
  "Verify M-p scrolls down by line."
  :tags '(unit fast core keybindings)
  (should (eq (key-binding (kbd "M-p")) 'scroll-down-line)))

(ert-deftest test-m-n-scroll-up ()
  "Verify M-n scrolls up by line."
  :tags '(unit fast core keybindings)
  (should (eq (key-binding (kbd "M-n")) 'scroll-up-line)))

;;; Built-in Modes

(ert-deftest test-savehist-mode ()
  "Verify savehist-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'savehist-mode)))

(ert-deftest test-recentf-mode ()
  "Verify recentf-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'recentf-mode)))

(ert-deftest test-global-auto-revert-mode ()
  "Verify global-auto-revert-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'global-auto-revert-mode)))

(ert-deftest test-repeat-mode ()
  "Verify repeat-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'repeat-mode)))

(ert-deftest test-delete-selection-mode ()
  "Verify delete-selection-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'delete-selection-mode)))

(ert-deftest test-electric-pair-mode ()
  "Verify electric-pair-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'electric-pair-mode)))

;;; Essential Packages

(ert-deftest test-super-save-mode ()
  "Verify super-save-mode is active."
  :tags '(unit fast core)
  (should (test-helper-mode-active-p 'super-save-mode)))

(ert-deftest test-super-save-configuration ()
  "Verify super-save configuration."
  :tags '(unit fast core)
  (should (eq super-save-auto-save-when-idle t))
  (should (eq super-save-remote-files nil))
  (should (eq super-save-silent t)))

(ert-deftest test-vundo-available ()
  "Verify vundo is available."
  :tags '(unit fast core)
  (should (commandp 'vundo))
  (should (eq (key-binding (kbd "C-x u")) 'vundo)))

(ert-deftest test-expand-region-available ()
  "Verify expand-region is available."
  :tags '(unit fast core)
  (should (commandp 'er/expand-region))
  (should (eq (key-binding (kbd "C-=")) 'er/expand-region)))

(ert-deftest test-multiple-cursors-available ()
  "Verify multiple-cursors is available."
  :tags '(unit fast core)
  (should (commandp 'mc/mark-next-like-this))
  (should (commandp 'mc/mark-previous-like-this))
  (should (eq (key-binding (kbd "C->")) 'mc/mark-next-like-this))
  (should (eq (key-binding (kbd "C-<")) 'mc/mark-previous-like-this)))

(ert-deftest test-drag-stuff-mode ()
  "Verify drag-stuff is configured."
  :tags '(unit core)
  (test-helper-with-temp-buffer-mode 'text-mode
    (should (test-helper-mode-active-p 'drag-stuff-mode))))

;;; Dired Configuration

(ert-deftest test-dired-configuration ()
  "Verify dired configuration."
  :tags '(unit fast core)
  (should (eq dired-dwim-target t))
  (should (eq dired-recursive-copies 'always))
  (should (eq dired-recursive-deletes 'top)))

(ert-deftest test-ibuffer-binding ()
  "Verify ibuffer binding."
  :tags '(unit fast core keybindings)
  (should (eq (key-binding (kbd "C-x C-b")) 'ibuffer)))

;;; Window Management

(ert-deftest test-window-split-preferences ()
  "Verify window split preferences."
  :tags '(unit fast core)
  (should (eq split-width-threshold 170))
  (should (eq split-height-threshold nil)))

(provide 'test-core)
;;; test-core.el ends here
