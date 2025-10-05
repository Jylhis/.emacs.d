;;; test-writing.el --- Test writing configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for writing and documentation tools including org-mode and exporters.

;;; Code:

(require 'test-helper)
(require 'writing)

;;; Org-mode Basic Configuration

(ert-deftest test-org-directory ()
  "Verify org-directory is set."
  :tags '(unit fast writing)
  (should (boundp 'org-directory))
  (should (stringp org-directory)))

(ert-deftest test-org-pretty-entities ()
  "Verify org-pretty-entities is enabled."
  :tags '(unit fast writing)
  (should (eq org-pretty-entities t)))

(ert-deftest test-org-clock-persist ()
  "Verify org-clock persistence."
  :tags '(unit fast writing)
  (should (eq org-clock-persist 'history)))

;;; Org Keybindings

(ert-deftest test-org-keybindings ()
  "Verify org-mode keybindings."
  :tags '(unit fast writing keybindings)
  (should (eq (key-binding (kbd "C-c a")) 'org-agenda))
  (should (eq (key-binding (kbd "C-c c")) 'org-capture)))

;;; Org Hooks

(ert-deftest test-org-visual-line-hook ()
  "Verify visual-line-mode hook for org-mode."
  :tags '(unit writing)
  (should (member 'visual-line-mode org-mode-hook)))

(ert-deftest test-org-font-setup-hook ()
  "Verify font setup hook for org-mode."
  :tags '(unit writing)
  (should (member 'j10s-setup-org-fonts org-mode-hook)))

;;; Org Font Setup Function

(ert-deftest test-org-font-setup-function ()
  "Verify org font setup function exists."
  :tags '(unit fast writing)
  (should (fboundp 'j10s-setup-org-fonts)))

(ert-deftest test-org-font-setup-configures-faces ()
  "Verify org font setup configures faces."
  :tags '(unit writing)
  (test-helper-with-temp-buffer-mode 'org-mode
    ;; After org-mode loads, our font setup should have run
    (should (facep 'org-table))
    (should (facep 'org-code))
    (should (facep 'org-block))))

;;; Org-appear

(ert-deftest test-org-appear-hook ()
  "Verify org-appear-mode hook."
  :tags '(unit writing)
  (when (test-helper-package-available-p 'org-appear)
    (should (member 'org-appear-mode org-mode-hook))))

;;; Org-modern

(ert-deftest test-org-modern ()
  "Verify org-modern is configured."
  :tags '(unit writing)
  (when (test-helper-package-available-p 'org-modern)
    (should (test-helper-mode-active-p 'global-org-modern-mode))))

;;; Org Exporters

(ert-deftest test-org-exporters-available ()
  "Verify org exporters are available."
  :tags '(unit writing)
  (should (test-helper-package-available-p 'ox-slack))
  (should (test-helper-package-available-p 'ox-jira))
  (should (test-helper-package-available-p 'ox-hugo))
  (should (test-helper-package-available-p 'ox-gfm)))

;;; Utils Integration

(ert-deftest test-org-agenda-setup-function ()
  "Verify org-agenda setup function exists."
  :tags '(unit fast writing)
  (should (fboundp 'my/setup-org-agenda-files))
  (should (fboundp 'my/update-org-agenda-files))
  (should (fboundp 'my/find-org-files-recursively)))

(ert-deftest test-org-agenda-files-updated ()
  "Verify org-agenda-files is set after setup."
  :tags '(unit writing)
  (should (boundp 'org-agenda-files))
  (should (listp org-agenda-files)))

(provide 'test-writing)
;;; test-writing.el ends here
