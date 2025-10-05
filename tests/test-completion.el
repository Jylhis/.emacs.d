;;; test-completion.el --- Test completion framework -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for modern completion framework using vertico, corfu, consult, embark, and orderless.

;;; Code:

(require 'test-helper)

;;; Vertico

(ert-deftest test-vertico-mode ()
  "Verify vertico-mode is active."
  :tags '(unit fast completion)
  (should (test-helper-mode-active-p 'vertico-mode)))

(ert-deftest test-vertico-directory-keybindings ()
  "Verify vertico-directory keybindings."
  :tags '(unit fast completion keybindings)
  (with-temp-buffer
    (let ((map vertico-map))
      (should (keymapp map))
      (should (lookup-key map (kbd "RET")))
      (should (lookup-key map (kbd "DEL")))
      (should (lookup-key map (kbd "M-DEL"))))))

(ert-deftest test-vertico-multiform-mode ()
  "Verify vertico-multiform-mode is active."
  :tags '(unit completion)
  (should (test-helper-mode-active-p 'vertico-multiform-mode)))

;;; Corfu

(ert-deftest test-global-corfu-mode ()
  "Verify global-corfu-mode is active."
  :tags '(unit fast completion)
  (should (test-helper-mode-active-p 'global-corfu-mode)))

(ert-deftest test-corfu-history-mode ()
  "Verify corfu-history-mode is active."
  :tags '(unit fast completion)
  (should (test-helper-mode-active-p 'corfu-history-mode)))

;;; Cape

(ert-deftest test-cape-completion-functions ()
  "Verify cape completion functions are registered."
  :tags '(unit fast completion)
  (should (member 'cape-file completion-at-point-functions))
  (should (member 'cape-dabbrev completion-at-point-functions))
  (should (member 'cape-keyword completion-at-point-functions)))

;;; Consult

(ert-deftest test-consult-keybindings ()
  "Verify consult keybindings."
  :tags '(unit fast completion keybindings)
  (should (eq (key-binding (kbd "C-x b")) 'consult-buffer))
  (should (eq (key-binding (kbd "C-s")) 'consult-line))
  (should (eq (key-binding (kbd "M-y")) 'consult-yank-pop))
  (should (eq (key-binding (kbd "M-g g")) 'consult-goto-line))
  (should (eq (key-binding (kbd "M-g i")) 'consult-imenu))
  (should (eq (key-binding (kbd "M-s r")) 'consult-ripgrep))
  (should (eq (key-binding (kbd "M-s g")) 'consult-grep)))

(ert-deftest test-consult-xref-integration ()
  "Verify consult integrates with xref."
  :tags '(unit fast completion)
  (should (eq xref-show-xrefs-function 'consult-xref))
  (should (eq xref-show-definitions-function 'consult-xref)))

;;; Orderless

(ert-deftest test-orderless-completion-style ()
  "Verify orderless is in completion-styles."
  :tags '(unit fast completion)
  (should (member 'orderless completion-styles)))

(ert-deftest test-completion-styles ()
  "Verify completion styles configuration."
  :tags '(unit fast completion)
  (should (listp completion-styles))
  (should (member 'orderless completion-styles))
  (should (member 'basic completion-styles)))

;;; Marginalia

(ert-deftest test-marginalia-mode ()
  "Verify marginalia-mode is active."
  :tags '(unit fast completion)
  (should (test-helper-mode-active-p 'marginalia-mode)))

;;; Embark

(ert-deftest test-embark-keybindings ()
  "Verify embark keybindings."
  :tags '(unit fast completion keybindings)
  (should (eq (key-binding (kbd "C-.")) 'embark-act))
  (should (eq (key-binding (kbd "C-;")) 'embark-dwim))
  (should (eq (key-binding (kbd "C-h B")) 'embark-bindings)))

(ert-deftest test-embark-prefix-help ()
  "Verify embark is configured as prefix-help-command."
  :tags '(unit fast completion)
  (should (eq prefix-help-command 'embark-prefix-help-command)))

(ert-deftest test-embark-consult-integration ()
  "Verify embark-consult integration."
  :tags '(unit completion)
  (should (test-helper-package-available-p 'embark-consult)))

;;; Avy

(ert-deftest test-avy-keybindings ()
  "Verify avy keybindings."
  :tags '(unit fast completion keybindings)
  (should (eq (key-binding (kbd "M-g c")) 'avy-goto-char))
  (should (eq (key-binding (kbd "M-g l")) 'avy-goto-line))
  (should (eq (key-binding (kbd "M-g w")) 'avy-goto-word-1)))

(ert-deftest test-avy-all-windows ()
  "Verify avy is configured for all windows."
  :tags '(unit fast completion)
  (should (eq avy-all-windows 'all-frames)))

;;; Zoxide

(ert-deftest test-zoxide-keybindings ()
  "Verify zoxide keybindings."
  :tags '(unit completion keybindings)
  (when (test-helper-package-available-p 'zoxide)
    (should (eq (key-binding (kbd "M-g z")) 'zoxide-find-file))))

(ert-deftest test-zoxide-find-file-hook ()
  "Verify zoxide hook for find-file."
  :tags '(unit completion)
  (when (test-helper-package-available-p 'zoxide)
    (should (member 'zoxide-add find-file-hook))))

;;; Completion Integration

(ert-deftest test-completion-framework-integration ()
  "Verify completion framework components work together."
  :tags '(unit completion)
  (should (test-helper-mode-active-p 'vertico-mode))
  (should (test-helper-mode-active-p 'global-corfu-mode))
  (should (test-helper-mode-active-p 'marginalia-mode))
  (should (member 'orderless completion-styles)))

(provide 'test-completion)
;;; test-completion.el ends here
