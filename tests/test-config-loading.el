;;; test-config-loading.el --- Test configuration module loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that verify all configuration modules load without errors.

;;; Code:

(require 'test-helper)

;; Load init.el to test that all modules can be loaded
;; This is safe because test-helper has mocked package.el
(load (expand-file-name "init.el" test-user-emacs-directory))

(ert-deftest test-platform-module-loads ()
  "Verify platform module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'platform)))

(ert-deftest test-core-module-loads ()
  "Verify core module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'core)))

(ert-deftest test-fonts-module-loads ()
  "Verify fonts module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'fonts)))

(ert-deftest test-ui-module-loads ()
  "Verify ui module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'ui)))

(ert-deftest test-completion-module-loads ()
  "Verify completion module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'completion)))

(ert-deftest test-programming-module-loads ()
  "Verify programming module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'programming)))

(ert-deftest test-per-project-module-loads ()
  "Verify per-project module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'per-project)))

(ert-deftest test-writing-module-loads ()
  "Verify writing module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'writing)))

(ert-deftest test-git-module-loads ()
  "Verify git module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'git)))

(ert-deftest test-help-module-loads ()
  "Verify help module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'help)))

(ert-deftest test-ai-module-loads ()
  "Verify ai module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'ai)))

(ert-deftest test-systems-module-loads ()
  "Verify systems module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'systems)))

(ert-deftest test-platforms-module-loads ()
  "Verify platforms module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'platforms)))

(ert-deftest test-app-launchers-module-loads ()
  "Verify app-launchers module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'app-launchers)))

(ert-deftest test-utils-module-loads ()
  "Verify utils module loads correctly."
  :tags '(unit fast config)
  (should (test-helper-feature-loaded-p 'utils)))

(ert-deftest test-init-file-loads ()
  "Verify init.el loaded successfully."
  :tags '(unit fast config)
  (should (featurep 'init))
  (should (boundp 'user-emacs-directory)))

(ert-deftest test-all-required-modules-loaded ()
  "Verify all required modules from init.el are loaded."
  :tags '(unit config)
  (let ((required-modules '(platform core fonts ui completion programming
                            per-project writing git help ai systems platforms
                            app-launchers)))
    (dolist (module required-modules)
      (should (featurep module)))))

(provide 'test-config-loading)
;;; test-config-loading.el ends here
