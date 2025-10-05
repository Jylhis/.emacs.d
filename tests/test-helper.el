;;; test-helper.el --- Test utilities and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Common setup and utilities for all tests.
;; Provides test sandbox, mocking helpers, and shared test infrastructure.

;;; Code:

(require 'ert)

;; Set up paths WITHOUT loading init.el
(defvar test-user-emacs-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "Path to the Emacs configuration directory for testing.")

;; Add load paths for modules and utilities
(add-to-list 'load-path test-user-emacs-directory)
(add-to-list 'load-path (expand-file-name "config" test-user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" test-user-emacs-directory))

;; Keep user-emacs-directory pointing to the real config directory
;; This is needed for (require 'utils) and other relative requires to work
(setq user-emacs-directory test-user-emacs-directory)

;; Initialize package system to set up autoloads
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Load use-package (built-in in Emacs 29+)
;; Required for config modules that use use-package declarations
(require 'use-package)
;; Disable automatic package installation in tests
(setq use-package-always-ensure nil)

;; Mock package.el to prevent installation in tests
(defun test-helper-mock-package-system ()
  "Mock package.el functions to prevent installation in tests."
  ;; Prevent package-vc-install from trying to install packages
  (advice-add 'package-vc-install :override #'ignore)
  (advice-add 'package-refresh-contents :override #'ignore)
  (advice-add 'package-install :override #'ignore)
  ;; Prevent package initialization
  (setq package-enable-at-startup nil))

;; Always mock package system in tests
(test-helper-mock-package-system)

;; Mock write operations to prevent errors in read-only Nix sandbox
(defun test-helper-mock-write-operations ()
  "Mock file write operations that fail in read-only environments."
  ;; Mock savehist save function
  (advice-add 'savehist-save :override #'ignore)
  ;; Mock recentf save function
  (advice-add 'recentf-save-list :override #'ignore)
  ;; Mock super-save
  (advice-add 'super-save-command :override #'ignore)
  (advice-add 'super-save-command-advice :override #'ignore)
  ;; Mock org-clock-save
  (advice-add 'org-clock-save :override #'ignore))

(test-helper-mock-write-operations)

;; Load only platform detection (required by many modules)
(require 'platform)

;; Individual test files will load modules as needed using (require 'module-name)

;;; Test Sandbox Utilities

(defvar test-sandbox-path
  (expand-file-name "sandbox" (file-name-directory load-file-name))
  "Path to temporary directory for test sandboxing.")

(defmacro with-test-sandbox (&rest body)
  "Execute BODY in isolated temporary directory.
Creates a fresh sandbox directory, executes BODY with default-directory
set to the sandbox, then cleans up the sandbox directory."
  `(let ((default-directory test-sandbox-path))
     (when (file-directory-p test-sandbox-path)
       (delete-directory test-sandbox-path t))
     (make-directory test-sandbox-path t)
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p test-sandbox-path)
         (delete-directory test-sandbox-path t)))))

(defun test-helper-create-temp-file (filename content)
  "Create temporary file FILENAME with CONTENT in test sandbox.
Returns the absolute path to the created file."
  (let ((file-path (expand-file-name filename test-sandbox-path)))
    (make-directory (file-name-directory file-path) t)
    (with-temp-file file-path
      (insert content))
    file-path))

;;; Git Repository Helpers

(defun test-helper-create-git-repo ()
  "Create a test git repository in the sandbox.
Requires git to be available. Returns sandbox directory path."
  (unless (executable-find "git")
    (error "Git is required for git-related tests"))
  (make-directory test-sandbox-path t)
  (let ((default-directory test-sandbox-path))
    (shell-command "git init" nil nil)
    (shell-command "git config user.name 'Test User'" nil nil)
    (shell-command "git config user.email 'test@example.com'" nil nil))
  test-sandbox-path)

(defun test-helper-git-add-file (filename content)
  "Add file FILENAME with CONTENT to test git repo and commit it."
  (let ((default-directory test-sandbox-path))
    (test-helper-create-temp-file filename content)
    (shell-command (format "git add %s" filename) nil nil)
    (shell-command (format "git commit -m 'Add %s'" filename) nil nil)))

;;; Mocking Utilities

(defmacro test-helper-with-temp-buffer-mode (mode &rest body)
  "Create temporary buffer, activate MODE, and execute BODY.
The buffer is killed after BODY completes."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

(defmacro test-helper-mock-completing-read (return-value &rest body)
  "Mock completing-read to return RETURN-VALUE and execute BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) ,return-value)))
     ,@body))

(defmacro test-helper-with-file-buffer (filename content &rest body)
  "Create temporary file with FILENAME and CONTENT, open in buffer, execute BODY.
File and buffer are cleaned up after BODY completes."
  (declare (indent 2))
  `(with-test-sandbox
     (let* ((file-path (test-helper-create-temp-file ,filename ,content))
            (buffer (find-file-noselect file-path)))
       (unwind-protect
           (with-current-buffer buffer
             ,@body)
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (when (file-exists-p file-path)
           (delete-file file-path))))))

;;; Assertion Helpers

(defun test-helper-feature-loaded-p (feature)
  "Check if FEATURE is loaded and return helpful message."
  (if (featurep feature)
      (message "✓ Feature '%s' is loaded" feature)
    (message "✗ Feature '%s' is NOT loaded" feature))
  (featurep feature))

(defun test-helper-mode-active-p (mode)
  "Check if MODE is active and return helpful message."
  (let ((active (and (boundp mode) (symbol-value mode))))
    (if active
        (message "✓ Mode '%s' is active" mode)
      (message "✗ Mode '%s' is NOT active" mode))
    active))

(defun test-helper-package-available-p (package)
  "Check if PACKAGE is available."
  (locate-library (symbol-name package)))

;;; Test Environment Variables

(defun test-helper-in-ci-p ()
  "Return non-nil if running in CI environment."
  (or (getenv "CI")
      (getenv "GITHUB_ACTIONS")))

(defun test-helper-quick-test-p ()
  "Return non-nil if QUICK_TEST environment variable is set."
  (getenv "QUICK_TEST"))

;;; Test Skipping Helpers

(defmacro test-helper-skip-in-ci (&rest body)
  "Skip BODY if running in CI environment."
  `(unless (test-helper-in-ci-p)
     ,@body))

(defmacro test-helper-skip-if-quick (&rest body)
  "Skip BODY if QUICK_TEST is set."
  `(unless (test-helper-quick-test-p)
     ,@body))

(provide 'test-helper)
;;; test-helper.el ends here
