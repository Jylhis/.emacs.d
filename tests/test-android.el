;;; test-android.el --- Test Android/Termux configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Android-specific configuration.
;; Most tests skip unless platform-android-p is true.

;;; Code:

(require 'test-helper)

;;; Platform Detection

(ert-deftest test-android-platform-detection ()
  "Verify Android platform detection works."
  :tags '(unit fast android platform)
  (should (booleanp platform-android-p)))

;;; Android-specific Tests (skip unless on Android)

(ert-deftest test-android-module-loads ()
  "Verify android module loads when on Android."
  :tags '(unit android)
  (skip-unless platform-android-p)
  (should (featurep 'android)))

(ert-deftest test-android-volume-key-bindings ()
  "Verify volume key bindings are registered on Android."
  :tags '(unit android keybindings)
  (skip-unless platform-android-p)
  (skip-unless (getenv "TERMUX_VERSION"))
  ;; Volume keys should be bound to modifiers or scroll commands
  (should (or (key-binding (kbd "<volume-up>"))
              (key-binding (kbd "<volume-down>")))))

(ert-deftest test-android-font-size-adaptation ()
  "Verify font size adapts to screen size on Android."
  :tags '(unit android)
  (skip-unless platform-android-p)
  (should (fboundp 'android-phone-p))
  (should (fboundp 'android-tablet-p)))

(ert-deftest test-android-storage-integration ()
  "Verify org-directory uses EXTERNAL_STORAGE on Android."
  :tags '(unit android)
  (skip-unless platform-android-p)
  (when (getenv "EXTERNAL_STORAGE")
    (should (stringp org-directory))
    (should (string-match-p "syncthing" org-directory))))

(ert-deftest test-android-performance-optimizations ()
  "Verify Android performance optimizations."
  :tags '(unit android)
  (skip-unless platform-android-p)
  (should (eq gc-cons-threshold (* 20 1024 1024))) ; 20MB
  (should (eq read-process-output-max (* 1024 1024)))) ; 1MB

(ert-deftest test-android-disabled-packages ()
  "Verify problematic packages are disabled on Android."
  :tags '(unit android)
  (skip-unless platform-android-p)
  (when (boundp 'android-disabled-packages)
    (should (listp android-disabled-packages))
    (should (member 'vterm android-disabled-packages))
    (should (member 'pdf-tools android-disabled-packages))))

(ert-deftest test-android-helper-functions ()
  "Verify Android helper functions exist."
  :tags '(unit fast android)
  (skip-unless platform-android-p)
  (should (fboundp 'android-restart-emacs))
  (should (commandp 'android-show-platform-info)))

(ert-deftest test-android-window-navigation ()
  "Verify Android-friendly window navigation."
  :tags '(unit android keybindings)
  (skip-unless platform-android-p)
  (should (eq (key-binding (kbd "C-<tab>")) 'other-window)))

(ert-deftest test-android-menu-bar ()
  "Verify Android custom menu exists."
  :tags '(unit android)
  (skip-unless platform-android-p)
  (when (display-graphic-p)
    (should (boundp 'android-menu))))

;;; Platform-specific Settings

(ert-deftest test-platforms-module ()
  "Verify platforms module loads for all platforms."
  :tags '(unit fast platform)
  (should (featurep 'platforms)))

(ert-deftest test-platform-disabled-packages ()
  "Verify platform-disabled-packages is set when appropriate."
  :tags '(unit platform)
  (when (or platform-android-p platform-windows-p)
    (should (boundp 'platform-disabled-packages))
    (should (listp platform-disabled-packages))))

(ert-deftest test-macos-specific-settings ()
  "Verify macOS-specific settings when on macOS."
  :tags '(unit platform)
  (skip-unless platform-macos-p)
  (when platform-macos-p
    (should (eq mac-command-modifier 'meta))
    (should (eq mac-option-modifier 'super))))

(ert-deftest test-linux-specific-settings ()
  "Verify Linux-specific settings when on Linux."
  :tags '(unit platform)
  (skip-unless platform-linux-p)
  (when platform-linux-p
    (should (eq browse-url-browser-function 'browse-url-xdg-open))))

(provide 'test-android)
;;; test-android.el ends here
