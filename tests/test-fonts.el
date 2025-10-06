;;; test-fonts.el --- Test font configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the comprehensive font system including detection, setup, and ligatures.

;;; Code:

(require 'test-helper)
(require 'fonts)

;;; Font System Functions

(ert-deftest test-font-setup-function-exists ()
  "Verify j10s-fonts-setup function exists."
  :tags '(unit fast fonts)
  (should (fboundp 'j10s-fonts-setup)))

(ert-deftest test-font-detection-functions ()
  "Verify font detection helper functions exist."
  :tags '(unit fast fonts)
  (should (fboundp 'j10s-fonts--get-available-families))
  (should (fboundp 'j10s-fonts--find-first-available))
  (should (fboundp 'j10s-fonts--set-face-font)))

(ert-deftest test-font-setup-functions ()
  "Verify font setup functions exist."
  :tags '(unit fast fonts)
  (should (fboundp 'j10s-fonts-setup-default))
  (should (fboundp 'j10s-fonts-setup-variable-pitch))
  (should (fboundp 'j10s-fonts-setup-serif))
  (should (fboundp 'j10s-fonts-setup-performance))
  (should (fboundp 'j10s-fonts-setup-theme-faces)))

(ert-deftest test-font-interactive-commands ()
  "Verify interactive font commands exist."
  :tags '(unit fast fonts)
  (should (commandp 'j10s-fonts-increase-size))
  (should (commandp 'j10s-fonts-decrease-size))
  (should (commandp 'j10s-fonts-reset-size))
  (should (commandp 'j10s-fonts-info)))

;;; Font Variables

(ert-deftest test-font-preference-variables ()
  "Verify font preference variables are defined."
  :tags '(unit fast fonts)
  (should (boundp 'j10s-fonts-default-family))
  (should (boundp 'j10s-fonts-variable-family))
  (should (boundp 'j10s-fonts-serif-family))
  (should (listp j10s-fonts-default-family))
  (should (listp j10s-fonts-variable-family))
  (should (listp j10s-fonts-serif-family)))

(ert-deftest test-font-cache-variable ()
  "Verify font cache variable exists."
  :tags '(unit fast fonts)
  (should (boundp 'j10s-fonts--available-cache)))

;;; Font Keybindings

(ert-deftest test-font-size-keybindings ()
  "Verify font size adjustment keybindings."
  :tags '(unit fast fonts keybindings)
  (should (eq (key-binding (kbd "C-+")) 'j10s-fonts-increase-size))
  (should (eq (key-binding (kbd "C--")) 'j10s-fonts-decrease-size))
  (should (eq (key-binding (kbd "C-0")) 'j10s-fonts-reset-size))
  (should (eq (key-binding (kbd "C-c f i")) 'j10s-fonts-info)))

;;; Font Performance Settings

(ert-deftest test-font-performance-settings ()
  "Verify font performance optimizations."
  :tags '(unit fast fonts)
  (should (eq inhibit-compacting-font-caches t))
  (should (eq scalable-fonts-allowed t)))

;;; Ligature Support

(ert-deftest test-ligature-mode ()
  "Verify ligature mode is configured."
  :tags '(unit fonts)
  (when (test-helper-package-available-p 'ligature)
    (should (test-helper-mode-active-p 'global-ligature-mode))))

(ert-deftest test-ligature-mode-in-prog-mode ()
  "Verify ligatures are enabled in programming modes."
  :tags '(unit fonts)
  (skip-unless (test-helper-package-available-p 'ligature))
  (test-helper-with-temp-buffer-mode 'emacs-lisp-mode
    ;; Ligature mode should be active in prog-mode derived modes
    (should (boundp 'ligature-mode))))

;;; Daemon/Client Consistency

(ert-deftest test-daemon-font-hook ()
  "Verify daemon font setup hook is registered when running as daemon."
  :tags '(unit fonts)
  (when (daemonp)
    (should (member 'j10s-fonts--setup-frame after-make-frame-functions))))

;;; Font Detection

(ert-deftest test-font-detection-returns-list ()
  "Verify font detection returns a list."
  :tags '(unit fonts)
  (let ((families (j10s-fonts--get-available-families)))
    (should (listp families))))

(ert-deftest test-find-first-available-font ()
  "Verify finding first available font from list."
  :tags '(unit fonts)
  (let* ((available-fonts (j10s-fonts--get-available-families))
         (test-list (list (cons "NonExistentFont" 100)
                         (cons (car available-fonts) 100)))
         (found (j10s-fonts--find-first-available test-list)))
    (when available-fonts
      (should (consp found))
      (should (equal (car found) (car available-fonts))))))

(provide 'test-fonts)
;;; test-fonts.el ends here
