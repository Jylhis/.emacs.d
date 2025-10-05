;;; test-lsp-workflow.el --- Test LSP workflow integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for LSP (eglot) workflow.
;; Tests eglot activation, keybindings, and integration with flymake.

;;; Code:

(require 'test-helper)
;; Integration tests need full config loaded (load only if not already loaded)
(unless (featurep 'init)
  (load (expand-file-name "init.el" test-user-emacs-directory)))

;;; Eglot Activation

(ert-deftest test-eglot-mode-activation ()
  "Test eglot-ensure is configured for prog-modes."
  :tags '(integration lsp)
  (should (member 'prog-mode-hook (mapcar #'car hook-list))))

(ert-deftest test-eglot-in-python-mode ()
  "Test eglot activates in python-mode."
  :tags '(integration lsp)
  (test-helper-with-temp-buffer-mode 'python-mode
    ;; Eglot should be in the hook
    (should (member 'eglot-ensure python-mode-hook))))

(ert-deftest test-eglot-exclusions ()
  "Test eglot doesn't activate in excluded modes."
  :tags '(integration lsp)
  (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
    ;; Emacs lisp should not use eglot by default
    (should (derived-mode-p 'emacs-lisp-mode))))

;;; Eglot Keybindings

(ert-deftest test-eglot-keybindings-available ()
  "Test eglot keybindings are registered."
  :tags '(integration lsp keybindings)
  (let ((map eglot-mode-map))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "C-c r r")) 'eglot-rename))
    (should (eq (lookup-key map (kbd "C-c r f")) 'eglot-format))
    (should (eq (lookup-key map (kbd "C-c r a")) 'eglot-code-actions))))

;;; Eglot-Flymake Integration

(ert-deftest test-eglot-flymake-integration ()
  "Test eglot integrates with flymake."
  :tags '(integration lsp)
  ;; Eglot automatically enables flymake
  (should (fboundp 'flymake-mode)))

(ert-deftest test-flymake-keybindings ()
  "Test flymake keybindings for LSP diagnostics."
  :tags '(integration lsp keybindings)
  (let ((map flymake-mode-map))
    (should (keymapp map))
    (should (eq (lookup-key map (kbd "M-n")) 'flymake-goto-next-error))
    (should (eq (lookup-key map (kbd "M-p")) 'flymake-goto-prev-error))))

;;; LSP Server Configuration

(ert-deftest test-lsp-server-programs ()
  "Test LSP server programs are configured."
  :tags '(integration lsp)
  (should (assoc '(go-mode go-ts-mode) eglot-server-programs)))

;;; Consult-Eglot Integration

(ert-deftest test-consult-eglot-available ()
  "Test consult-eglot provides symbol search."
  :tags '(integration lsp)
  (when (test-helper-package-available-p 'consult-eglot)
    (should (commandp 'consult-eglot-symbols))
    (let ((map eglot-mode-map))
      (should (eq (lookup-key map (kbd "C-M-.")) 'consult-eglot-symbols)))))

;;; LSP with Real Buffer (requires LSP server - skip in CI)

(ert-deftest test-eglot-in-python-buffer ()
  "Test eglot in actual python buffer."
  :tags '(integration lsp slow requires-external)
  (skip-unless (executable-find "pylsp"))
  (skip-unless (not (test-helper-in-ci-p)))
  (test-helper-with-file-buffer "test.py" "def test():\n    pass\n"
    (python-mode)
    ;; Hook should be present
    (should (member 'eglot-ensure python-mode-hook))))

(ert-deftest test-eglot-format-workflow ()
  "Test eglot format workflow."
  :tags '(integration lsp workflow)
  (should (commandp 'eglot-format))
  ;; Format command should be available
  (let ((map eglot-mode-map))
    (should (eq (lookup-key map (kbd "C-c r f")) 'eglot-format))))

(ert-deftest test-eglot-rename-workflow ()
  "Test eglot rename workflow."
  :tags '(integration lsp workflow)
  (should (commandp 'eglot-rename))
  (let ((map eglot-mode-map))
    (should (eq (lookup-key map (kbd "C-c r r")) 'eglot-rename))))

(ert-deftest test-eglot-code-actions-workflow ()
  "Test eglot code actions workflow."
  :tags '(integration lsp workflow)
  (should (commandp 'eglot-code-actions))
  (should (commandp 'eglot-code-action-organize-imports))
  (should (commandp 'eglot-code-action-quickfix)))

;;; Eldoc Integration

(ert-deftest test-eglot-eldoc-integration ()
  "Test eglot integrates with eldoc."
  :tags '(integration lsp)
  (should (member 'eldoc-mode eglot-managed-mode-hook)))

;;; Inlay Hints

(ert-deftest test-eglot-inlay-hints ()
  "Test eglot inlay hints configuration."
  :tags '(integration lsp)
  (when (fboundp 'eglot-inlay-hints-mode)
    (should (member 'eglot-managed-mode-hook (mapcar #'car hook-list)))))

(provide 'test-lsp-workflow)
;;; test-lsp-workflow.el ends here
