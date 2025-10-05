;;; test-per-project.el --- Test project management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for project management tools including projection and compile-multi.

;;; Code:

(require 'test-helper)
(require 'per-project)

;;; Project Configuration

(ert-deftest test-project-buffers-viewer ()
  "Verify project buffers viewer configuration."
  :tags '(unit fast per-project)
  (should (eq project-buffers-viewer 'project-list-buffers-ibuffer)))

;;; Projection

(ert-deftest test-global-projection-hook-mode ()
  "Verify global-projection-hook-mode is active."
  :tags '(unit per-project)
  (when (test-helper-package-available-p 'projection)
    (should (test-helper-mode-active-p 'global-projection-hook-mode))))

(ert-deftest test-projection-keybinding ()
  "Verify projection-map keybinding."
  :tags '(unit fast per-project keybindings)
  (when (test-helper-package-available-p 'projection)
    (let ((binding (key-binding (kbd "C-x P"))))
      (should (keymapp binding)))))

(ert-deftest test-projection-safe-variables ()
  "Verify projection commands are marked as safe local variables."
  :tags '(unit fast per-project)
  (when (test-helper-package-available-p 'projection)
    (should (get 'projection-commands-configure-project 'safe-local-variable))
    (should (get 'projection-commands-build-project 'safe-local-variable))
    (should (get 'projection-commands-test-project 'safe-local-variable))
    (should (get 'projection-commands-run-project 'safe-local-variable))
    (should (get 'projection-commands-package-project 'safe-local-variable))
    (should (get 'projection-commands-install-project 'safe-local-variable))))

;;; Projection-multi

(ert-deftest test-projection-multi-available ()
  "Verify projection-multi is available."
  :tags '(unit per-project)
  (should (test-helper-package-available-p 'projection-multi)))

(ert-deftest test-projection-multi-keybinding ()
  "Verify projection-multi-compile keybinding."
  :tags '(unit per-project keybindings)
  (when (test-helper-package-available-p 'projection-multi)
    (let ((map project-prefix-map))
      (should (keymapp map))
      (should (eq (lookup-key map (kbd "RET")) 'projection-multi-compile)))))

;;; Projection-multi-embark

(ert-deftest test-projection-multi-embark-available ()
  "Verify projection-multi-embark integration."
  :tags '(unit per-project)
  (when (and (test-helper-package-available-p 'projection-multi)
             (test-helper-package-available-p 'embark))
    (should (test-helper-package-available-p 'projection-multi-embark))))

;;; Compile-multi

(ert-deftest test-compile-multi-configuration ()
  "Verify compile-multi configuration."
  :tags '(unit fast per-project)
  (when (test-helper-package-available-p 'compile-multi)
    (should (boundp 'compile-multi-config))
    (should (listp compile-multi-config))))

(ert-deftest test-compile-multi-go-mode ()
  "Verify go-mode compile commands."
  :tags '(unit fast per-project)
  (when (boundp 'compile-multi-config)
    (let ((go-commands (cdr (assoc 'go-mode compile-multi-config))))
      (should go-commands)
      (should (assoc "go test" go-commands))
      (should (assoc "go build" go-commands)))))

(ert-deftest test-compile-multi-python-mode ()
  "Verify python-mode compile commands."
  :tags '(unit fast per-project)
  (when (boundp 'compile-multi-config)
    (let ((python-commands (cdr (assoc 'python-mode compile-multi-config))))
      (should python-commands)
      (should (assoc "pytest" python-commands)))))

(ert-deftest test-compile-multi-nix-mode ()
  "Verify nix-ts-mode compile commands."
  :tags '(unit fast per-project)
  (when (boundp 'compile-multi-config)
    (let ((nix-commands (cdr (assoc 'nix-ts-mode compile-multi-config))))
      (should nix-commands)
      (should (assoc "nix flake check" nix-commands))
      (should (assoc "nix fmt" nix-commands)))))

;;; Consult-compile-multi

(ert-deftest test-consult-compile-multi-mode ()
  "Verify consult-compile-multi-mode is active."
  :tags '(unit per-project)
  (when (test-helper-package-available-p 'consult-compile-multi)
    (should (test-helper-mode-active-p 'consult-compile-multi-mode))))

;;; Compile-multi-nerd-icons

(ert-deftest test-compile-multi-nerd-icons-available ()
  "Verify compile-multi-nerd-icons is available."
  :tags '(unit per-project)
  (should (test-helper-package-available-p 'compile-multi-nerd-icons)))

;;; Compile-multi-embark

(ert-deftest test-compile-multi-embark-mode ()
  "Verify compile-multi-embark-mode is active."
  :tags '(unit per-project)
  (when (and (test-helper-package-available-p 'compile-multi)
             (test-helper-package-available-p 'embark))
    (should (test-helper-mode-active-p 'compile-multi-embark-mode))))

(provide 'test-per-project)
;;; test-per-project.el ends here
