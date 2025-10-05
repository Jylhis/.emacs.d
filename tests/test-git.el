;;; test-git.el --- Test git configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for git integration with Magit, diff-hl, smerge, and ediff.

;;; Code:

(require 'test-helper)

;;; Magit

(ert-deftest test-magit-keybinding ()
  "Verify magit-status keybinding."
  :tags '(unit fast git keybindings)
  (should (eq (key-binding (kbd "C-c g")) 'magit-status)))

(ert-deftest test-magit-configuration ()
  "Verify magit configuration."
  :tags '(unit fast git)
  (should (eq magit-diff-refine-hunk t))
  (should (eq magit-diff-refine-ignore-whitespace t))
  (should (eq magit-diff-hide-trailing-cr-characters t))
  (should (eq magit-diff-context-lines 5)))

(ert-deftest test-magit-available ()
  "Verify magit commands are available."
  :tags '(unit fast git)
  (should (commandp 'magit-status))
  (should (commandp 'magit-dispatch))
  (should (commandp 'magit-log)))

;;; Magit Todos

(ert-deftest test-magit-todos-mode ()
  "Verify magit-todos-mode is active."
  :tags '(unit git)
  (when (test-helper-package-available-p 'magit-todos)
    (should (test-helper-mode-active-p 'magit-todos-mode))))

;;; Diff-hl

(ert-deftest test-global-diff-hl-mode ()
  "Verify global-diff-hl-mode is active."
  :tags '(unit git)
  (should (test-helper-mode-active-p 'global-diff-hl-mode)))

(ert-deftest test-diff-hl-show-hunk-mouse-mode ()
  "Verify diff-hl-show-hunk-mouse-mode is active."
  :tags '(unit git)
  (should (test-helper-mode-active-p 'global-diff-hl-show-hunk-mouse-mode)))

(ert-deftest test-diff-hl-configuration ()
  "Verify diff-hl configuration."
  :tags '(unit fast git)
  (should (eq diff-hl-draw-borders nil))
  (should (eq fringes-outside-margins t))
  (should (eq diff-hl-side 'left)))

(ert-deftest test-diff-hl-hooks ()
  "Verify diff-hl hooks."
  :tags '(unit git)
  (should (member 'diff-hl-dired-mode dired-mode-hook))
  (should (member 'diff-hl-magit-post-refresh magit-post-refresh-hook)))

(ert-deftest test-diff-hl-flydiff-mode ()
  "Verify diff-hl-flydiff-mode is active."
  :tags '(unit git)
  (should (test-helper-mode-active-p 'diff-hl-flydiff-mode)))

;;; Smerge Mode

(ert-deftest test-smerge-keybindings ()
  "Verify smerge-mode keybindings."
  :tags '(unit git keybindings)
  (let ((map smerge-mode-map))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "C-c ^ u")) 'smerge-keep-upper))
    (should (eq (lookup-key map (kbd "C-c ^ l")) 'smerge-keep-lower))
    (should (eq (lookup-key map (kbd "C-c ^ n")) 'smerge-next))
    (should (eq (lookup-key map (kbd "C-c ^ p")) 'smerge-prev))))

;;; Ediff

(ert-deftest test-ediff-configuration ()
  "Verify ediff configuration."
  :tags '(unit fast git)
  (should (eq ediff-window-setup-function 'ediff-setup-windows-plain))
  (should (eq ediff-split-window-function 'split-window-horizontally))
  (should (eq ediff-merge-split-window-function 'split-window-horizontally))
  (should (equal ediff-diff-options "-w"))
  (should (equal ediff-custom-diff-options "-u"))
  (should (eq ediff-merge-revisions-with-ancestor t)))

(ert-deftest test-ediff-control-frame ()
  "Verify ediff control frame parameters."
  :tags '(unit fast git)
  (should (assoc 'name ediff-control-frame-parameters))
  (should (eq (cdr (assoc 'width ediff-control-frame-parameters)) 60))
  (should (eq (cdr (assoc 'height ediff-control-frame-parameters)) 14)))

;;; Git Integration

(ert-deftest test-git-available ()
  "Verify git is available (skip if not)."
  :tags '(integration git requires-external)
  (skip-unless (executable-find "git"))
  (should (executable-find "git")))

(provide 'test-git)
;;; test-git.el ends here
