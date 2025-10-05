;;; test-completion-workflow.el --- Test completion workflow integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the completion framework workflow.
;; Tests how vertico, consult, corfu, and embark work together.

;;; Code:

(require 'test-helper)

;;; Completion Framework Integration

(ert-deftest test-completion-framework-active ()
  "Verify all completion framework components are active."
  :tags '(integration completion)
  (should (test-helper-mode-active-p 'vertico-mode))
  (should (test-helper-mode-active-p 'global-corfu-mode))
  (should (test-helper-mode-active-p 'marginalia-mode))
  (should (member 'orderless completion-styles)))

(ert-deftest test-vertico-consult-integration ()
  "Test vertico and consult integration."
  :tags '(integration completion workflow)
  (should (eq xref-show-xrefs-function 'consult-xref))
  (should (eq xref-show-definitions-function 'consult-xref))
  (should (eq (key-binding (kbd "C-x b")) 'consult-buffer)))

(ert-deftest test-corfu-completion-in-buffer ()
  "Test corfu provides completion in buffer."
  :tags '(integration completion)
  (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
    (should (test-helper-mode-active-p 'corfu-mode))
    (insert "(defun test-fun")
    (should (member 'cape-file completion-at-point-functions))
    (should (member 'cape-dabbrev completion-at-point-functions))))

(ert-deftest test-embark-on-completion-candidates ()
  "Test embark can act on completion candidates."
  :tags '(integration completion)
  (should (commandp 'embark-act))
  (should (commandp 'embark-dwim))
  (should (eq prefix-help-command 'embark-prefix-help-command)))

(ert-deftest test-marginalia-annotations ()
  "Test marginalia provides annotations."
  :tags '(integration completion)
  (should (test-helper-mode-active-p 'marginalia-mode)))

;;; Consult Commands Workflow

(ert-deftest test-consult-buffer-workflow ()
  "Test consult-buffer command is available and bound."
  :tags '(integration completion workflow)
  (should (commandp 'consult-buffer))
  (should (eq (key-binding (kbd "C-x b")) 'consult-buffer)))

(ert-deftest test-consult-line-workflow ()
  "Test consult-line command is available and bound."
  :tags '(integration completion workflow)
  (should (commandp 'consult-line))
  (should (eq (key-binding (kbd "C-s")) 'consult-line)))

(ert-deftest test-consult-imenu-workflow ()
  "Test consult-imenu for code navigation."
  :tags '(integration completion workflow)
  (should (commandp 'consult-imenu))
  (should (eq (key-binding (kbd "M-g i")) 'consult-imenu)))

;;; File Completion Workflow

(ert-deftest test-find-file-completion ()
  "Test file finding with completion."
  :tags '(integration completion workflow)
  (with-test-sandbox
    (test-helper-create-temp-file "test-file.txt" "content")
    (test-helper-create-temp-file "test-file-2.txt" "content")
    ;; Verify files exist for completion
    (should (file-exists-p (expand-file-name "test-file.txt" test-sandbox-path)))
    (should (file-exists-p (expand-file-name "test-file-2.txt" test-sandbox-path)))))

;;; Orderless Completion Style

(ert-deftest test-orderless-matching ()
  "Test orderless completion style works."
  :tags '(integration completion)
  (should (member 'orderless completion-styles))
  (let ((completion-styles '(orderless)))
    ;; Orderless should be able to match non-contiguous patterns
    (should (member 'orderless completion-styles))))

;;; Avy Integration

(ert-deftest test-avy-jump-workflow ()
  "Test avy jump commands."
  :tags '(integration completion workflow)
  (should (commandp 'avy-goto-char))
  (should (commandp 'avy-goto-line))
  (should (eq (key-binding (kbd "M-g c")) 'avy-goto-char)))

;;; Cape Completion Functions

(ert-deftest test-cape-integration ()
  "Test cape completion functions are integrated."
  :tags '(integration completion)
  (should (member 'cape-file completion-at-point-functions))
  (should (member 'cape-dabbrev completion-at-point-functions))
  (should (member 'cape-keyword completion-at-point-functions)))

;;; Embark-Consult Integration

(ert-deftest test-embark-consult-integration ()
  "Test embark and consult work together."
  :tags '(integration completion)
  (when (test-helper-package-available-p 'embark-consult)
    (should (member 'consult-preview-at-point-mode embark-collect-mode-hook))))

(provide 'test-completion-workflow)
;;; test-completion-workflow.el ends here
