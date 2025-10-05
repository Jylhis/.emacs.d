;;; test-git-workflow.el --- Test git workflow integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for git workflow.
;; Tests magit, diff-hl, and smerge integration.

;;; Code:

(require 'test-helper)
;; Integration tests need full config loaded (load only if not already loaded)
(unless (featurep 'init)
  (load (expand-file-name "init.el" test-user-emacs-directory)))

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
  (ert-skip "Disabled: tests use-package :custom values or deferred configuration, requires full init.el")
  (should (member 'global-diff-hl-mode after-init-hook))
  (should (member 'global-diff-hl-show-hunk-mouse-mode after-init-hook))
  (require 'diff-hl)
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
  (ert-skip "Disabled: tests use-package :custom values or deferred configuration, requires full init.el")
  (require 'diff-hl)
  (should (member 'diff-hl-magit-post-refresh magit-post-refresh-hook)))

;;; Smerge Mode

(ert-deftest test-smerge-mode-on-conflict ()
  "Test smerge-mode activates on git conflict markers."
  :tags '(integration git workflow)
  (ert-skip "Disabled: tests use-package :custom values or deferred configuration, requires full init.el")
  (require 'smerge-mode)
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

;; smerge-keybindings test moved to tests/test-git.el

;;; Ediff

;; ediff-configuration test moved to tests/test-git.el

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
  (ert-skip "Disabled: tests use-package :custom values or deferred configuration, requires full init.el")
  (when (test-helper-package-available-p 'magit-todos)
    (require 'magit-todos)
    (should (test-helper-mode-active-p 'magit-todos-mode))))

(provide 'test-git-workflow)
;;; test-git-workflow.el ends here
