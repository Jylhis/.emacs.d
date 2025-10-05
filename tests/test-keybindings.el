;;; test-keybindings.el --- Centralized keybinding tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive tests for all critical keybindings across the configuration.
;; This provides a single place to verify that important workflows have proper keybindings.

;;; Code:

(require 'test-helper)
;; Load init.el to have all keybindings registered
(load (expand-file-name "init.el" test-user-emacs-directory))

;;; Global Essential Keybindings

(ert-deftest test-essential-unbound-keys ()
  "Verify problematic default keybindings are unbound."
  :tags '(unit fast keybindings critical)
  (should (null (key-binding (kbd "C-z"))))      ; suspend-frame disabled
  (should (null (key-binding (kbd "C-x C-z")))))) ; suspend-frame disabled

(ert-deftest test-essential-navigation-keys ()
  "Verify essential navigation keybindings."
  :tags '(unit fast keybindings critical)
  (should (eq (key-binding (kbd "M-o")) 'other-window))
  (should (eq (key-binding (kbd "M-p")) 'scroll-down-line))
  (should (eq (key-binding (kbd "M-n")) 'scroll-up-line)))

;;; File and Buffer Management

(ert-deftest test-buffer-management-keys ()
  "Verify buffer management keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-x b")) 'consult-buffer))
  (should (eq (key-binding (kbd "C-x C-b")) 'ibuffer)))

(ert-deftest test-undo-redo-keys ()
  "Verify undo/redo keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-x u")) 'vundo)))

;;; Search and Navigation

(ert-deftest test-search-keys ()
  "Verify search keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-s")) 'consult-line))
  (should (eq (key-binding (kbd "M-g g")) 'consult-goto-line))
  (should (eq (key-binding (kbd "M-g i")) 'consult-imenu)))

(ert-deftest test-avy-keys ()
  "Verify avy jump keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "M-g c")) 'avy-goto-char))
  (should (eq (key-binding (kbd "M-g l")) 'avy-goto-line))
  (should (eq (key-binding (kbd "M-g w")) 'avy-goto-word-1)))

;;; Git

(ert-deftest test-git-keys ()
  "Verify git-related keybindings."
  :tags '(unit fast keybindings critical)
  (should (eq (key-binding (kbd "C-c g")) 'magit-status)))

;;; Org-mode

(ert-deftest test-org-keys ()
  "Verify org-mode keybindings."
  :tags '(unit fast keybindings critical)
  (should (eq (key-binding (kbd "C-c a")) 'org-agenda))
  (should (eq (key-binding (kbd "C-c c")) 'org-capture)))

;;; Help System

(ert-deftest test-help-keys ()
  "Verify help system keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-h f")) 'helpful-callable))
  (should (eq (key-binding (kbd "C-h v")) 'helpful-variable))
  (should (eq (key-binding (kbd "C-h k")) 'helpful-key)))

;;; Editing

(ert-deftest test-editing-keys ()
  "Verify editing keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-=")) 'er/expand-region))
  (should (eq (key-binding (kbd "C->")) 'mc/mark-next-like-this))
  (should (eq (key-binding (kbd "C-<")) 'mc/mark-previous-like-this)))

;;; Font Size

(ert-deftest test-font-size-keys ()
  "Verify font size adjustment keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-+")) 'j10s-fonts-increase-size))
  (should (eq (key-binding (kbd "C--")) 'j10s-fonts-decrease-size))
  (should (eq (key-binding (kbd "C-0")) 'j10s-fonts-reset-size)))

;;; Embark

(ert-deftest test-embark-keys ()
  "Verify embark keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-.")) 'embark-act))
  (should (eq (key-binding (kbd "C-;")) 'embark-dwim)))

;;; Window Management

(ert-deftest test-winner-keys ()
  "Verify winner-mode keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-c u")) 'winner-undo))
  (should (eq (key-binding (kbd "C-c r")) 'winner-redo)))

;;; Theme

(ert-deftest test-theme-keys ()
  "Verify theme toggle keybinding."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "C-c t")) 'modus-themes-toggle)))

;;; AI

(ert-deftest test-ai-keys ()
  "Verify AI integration keybindings."
  :tags '(unit fast keybindings)
  (when (test-helper-package-available-p 'claude-code-ide)
    (should (eq (key-binding (kbd "C-c C-'")) 'claude-code-ide-menu))))

;;; Project Management

(ert-deftest test-project-keys ()
  "Verify project management keybindings."
  :tags '(unit fast keybindings)
  (when (test-helper-package-available-p 'projection)
    (let ((binding (key-binding (kbd "C-x P"))))
      (should (keymapp binding)))))

;;; Yank/Paste

(ert-deftest test-yank-keys ()
  "Verify yank/paste keybindings."
  :tags '(unit fast keybindings)
  (should (eq (key-binding (kbd "M-y")) 'consult-yank-pop)))

;;; Zoxide

(ert-deftest test-zoxide-keys ()
  "Verify zoxide keybindings."
  :tags '(unit keybindings)
  (when (test-helper-package-available-p 'zoxide)
    (should (eq (key-binding (kbd "M-g z")) 'zoxide-find-file))))

;;; Critical Workflow Test

(ert-deftest test-critical-workflow-keybindings ()
  "Verify all critical workflow keybindings are set.
This test ensures that the most important daily-use keybindings are available."
  :tags '(unit keybindings critical)
  (let ((critical-bindings '(("C-c g" . magit-status)        ; Git
                             ("C-x b" . consult-buffer)       ; Buffer switching
                             ("C-s" . consult-line)           ; Search in buffer
                             ("M-o" . other-window)           ; Window navigation
                             ("C-c a" . org-agenda)           ; Org agenda
                             ("C-h f" . helpful-callable)     ; Help on function
                             ("C-=" . er/expand-region))))    ; Expand region
    (dolist (binding critical-bindings)
      (let ((key (car binding))
            (cmd (cdr binding)))
        (should (eq (key-binding (kbd key)) cmd))
        (message "✓ %s → %s" key cmd)))))

(provide 'test-keybindings)
;;; test-keybindings.el ends here
