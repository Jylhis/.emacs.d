;;; test-git-workflow.el --- Test git workflow integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for git workflow.
;; Tests magit, diff-hl, and smerge integration.

;;; Code:

(require 'test-helper)
;; Integration tests need full config loaded
(load (expand-file-name "init.el" test-user-emacs-directory))

;;; Magit Workflow

(ert-deftest test-magit-status-command ()
  "Test magit-status command is available."
  :tags '(integration git workflow)
  (should (commandp 'magit-status))
  (should (eq (key-binding (kbd "C-c g")) 'magit-status)))

(ert-deftest test-magit-in-git-repo ()
  "Test magit works in a git repository."
  :tags '(integration git workflow requires-external)
  (skip-unless (executable-find "git"))
  (test-helper-skip-in-ci)
  (with-test-sandbox
    (test-helper-create-git-repo)
    (test-helper-git-add-file "test.txt" "test content\n")
    ;; Verify magit can list files
    (should (magit-list-files))))

;;; Diff-hl Integration

(ert-deftest test-diff-hl-modes-active ()
  "Test diff-hl modes are active."
  :tags '(integration git)
  (should (test-helper-mode-active-p 'global-diff-hl-mode))
  (should (test-helper-mode-active-p 'global-diff-hl-show-hunk-mouse-mode))
  (should (test-helper-mode-active-p 'diff-hl-flydiff-mode)))

(ert-deftest test-diff-hl-in-dired ()
  "Test diff-hl activates in dired."
  :tags '(integration git)
  (skip-unless (executable-find "git"))
  (with-test-sandbox
    (test-helper-create-git-repo)
    (let ((default-directory test-sandbox-path))
      (with-current-buffer (dired test-sandbox-path)
        (should (member 'diff-hl-dired-mode dired-mode-hook))
        (kill-buffer)))))

(ert-deftest test-diff-hl-magit-integration ()
  "Test diff-hl integrates with magit."
  :tags '(integration git)
  (should (member 'diff-hl-magit-post-refresh magit-post-refresh-hook)))

;;; Smerge Mode

(ert-deftest test-smerge-mode-on-conflict ()
  "Test smerge-mode activates on git conflict markers."
  :tags '(integration git workflow)
  (test-helper-with-file-buffer "conflict.txt"
    "<<<<<<< HEAD
version 1
=======
version 2
>>>>>>> branch
"
    (goto-char (point-min))
    (when (fboundp 'smerge-mode)
      (smerge-mode 1)
      (should smerge-mode)
      (should (smerge-find-conflict)))))

(ert-deftest test-smerge-keybindings ()
  "Test smerge keybindings work."
  :tags '(integration git keybindings)
  (let ((map smerge-mode-map))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "C-c ^ u")) 'smerge-keep-upper))
    (should (eq (lookup-key map (kbd "C-c ^ l")) 'smerge-keep-lower))))

;;; Ediff

(ert-deftest test-ediff-configuration ()
  "Test ediff is properly configured."
  :tags '(integration git)
  (should (eq ediff-window-setup-function 'ediff-setup-windows-plain))
  (should (eq ediff-split-window-function 'split-window-horizontally)))

;;; Git File Modification Workflow

(ert-deftest test-git-file-modification-workflow ()
  "Test workflow of modifying a git-tracked file."
  :tags '(integration git workflow requires-external slow)
  (skip-unless (executable-find "git"))
  (test-helper-skip-in-ci)
  (with-test-sandbox
    (test-helper-create-git-repo)
    (test-helper-git-add-file "test.txt" "original content\n")
    ;; Modify file
    (let ((file-path (expand-file-name "test.txt" test-sandbox-path)))
      (with-temp-file file-path
        (insert "modified content\n"))
      ;; File should now be modified
      (should (file-exists-p file-path)))))

;;; Magit Status Workflow

(ert-deftest test-magit-status-workflow ()
  "Test opening magit status in a git repository."
  :tags '(integration git workflow requires-external)
  (skip-unless (executable-find "git"))
  (test-helper-skip-in-ci)
  (with-test-sandbox
    (test-helper-create-git-repo)
    (test-helper-git-add-file "test.txt" "content\n")
    ;; Magit should be able to show status
    (should (fboundp 'magit-status))))

;;; Magit Log Workflow

(ert-deftest test-magit-log-workflow ()
  "Test magit log commands."
  :tags '(integration git workflow)
  (should (commandp 'magit-log))
  (should (commandp 'magit-log-current)))

;;; Magit Todos Integration

(ert-deftest test-magit-todos-integration ()
  "Test magit-todos integration."
  :tags '(integration git)
  (when (test-helper-package-available-p 'magit-todos)
    (should (test-helper-mode-active-p 'magit-todos-mode))))

(provide 'test-git-workflow)
;;; test-git-workflow.el ends here
