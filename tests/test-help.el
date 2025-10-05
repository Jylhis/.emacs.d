;;; test-help.el --- Test help system configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for enhanced help and documentation tools.

;;; Code:

(require 'test-helper)
(require 'help)

;;; Help-at-pt

(ert-deftest test-help-at-pt-configuration ()
  "Verify help-at-pt configuration."
  :tags '(unit fast help)
  (require 'help-at-pt)
  (should (eq help-at-pt-display-when-idle t)))

;;; Helpful

(ert-deftest test-helpful-keybindings ()
  "Verify helpful keybindings."
  :tags '(unit fast help keybindings)
  (require 'helpful)
  (should (eq (key-binding (kbd "C-h f")) 'helpful-callable))
  (should (eq (key-binding (kbd "C-h v")) 'helpful-variable))
  (should (eq (key-binding (kbd "C-h k")) 'helpful-key))
  (should (eq (key-binding (kbd "C-c C-d")) 'helpful-at-point))
  (should (eq (key-binding (kbd "C-h F")) 'helpful-function))
  (should (eq (key-binding (kbd "C-h C")) 'helpful-command)))

(ert-deftest test-helpful-available ()
  "Verify helpful commands are available."
  :tags '(unit fast help)
  (should (commandp 'helpful-callable))
  (should (commandp 'helpful-variable))
  (should (commandp 'helpful-key))
  (should (commandp 'helpful-symbol)))

;;; Apropos

(ert-deftest test-apropos-configuration ()
  "Verify apropos is configured."
  :tags '(unit fast help)
  ;; Check that apropos package is available (configured via use-package in help.el)
  (should (locate-library "apropos")))

;;; Dash-docs

(ert-deftest test-dash-docs-configuration ()
  "Verify dash-docs configuration."
  :tags '(unit help)
  (when (test-helper-package-available-p 'dash-docs)
    (require 'dash-docs)
    (should (boundp 'dash-docs-docsets-path))
    (should (stringp dash-docs-docsets-path))
    (should (eq dash-docs-enable-debugging nil))
    (should (eq dash-docs-browser-func 'eww))))

(ert-deftest test-dash-docs-mode-hooks ()
  "Verify dash-docs mode-specific hooks."
  :tags '(unit help)
  (when (test-helper-package-available-p 'dash-docs)
    (require 'dash-docs)
    ;; Test that hooks are set up for specific modes
    (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
      (should (boundp 'dash-docs-docsets)))))

;;; Consult-dash

(ert-deftest test-consult-dash-available ()
  "Verify consult-dash is available."
  :tags '(unit help)
  (should (test-helper-package-available-p 'consult-dash)))

(provide 'test-help)
;;; test-help.el ends here
