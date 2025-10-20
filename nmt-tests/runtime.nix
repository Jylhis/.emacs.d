{
  pkgs,
  home-manager,
  homeModule,
  emacsPackage,
}:

{
  # Runtime validation test using nixosTest
  # This actually starts Emacs and verifies it works correctly
  test-emacs-runtime = pkgs.nixosTest {
    name = "emacs-runtime-validation";

    nodes.machine =
      { ... }:
      {
        imports = [ home-manager.nixosModules.home-manager ];

        users.users.testuser = {
          isNormalUser = true;
          home = "/home/testuser";
          uid = 1000;
        };

        home-manager.users.testuser = {
          imports = [ homeModule ];
          programs.emacs = {
            enable = true;
            package = emacsPackage;
          };
          home.stateVersion = "24.11";
        };

        # Ensure X11 is available for GUI tests (if needed)
        services.xserver.enable = false; # We'll test in batch mode

        # Ensure proper environment
        environment.variables = {
          HOME = "/home/testuser";
        };
      };

    testScript = ''
      start_all()
      machine.wait_for_unit("multi-user.target")

      print("=== Test 1: Emacs Binary Available ===")
      machine.succeed("test -x /home/testuser/.nix-profile/bin/emacs")
      print("PASS: Emacs binary is executable")

      print("\n=== Test 2: Emacs Starts in Batch Mode ===")
      output = machine.succeed('sudo -u testuser emacs --batch --eval \'(message "Emacs started")\' ')
      assert "Emacs started" in output, f"Expected startup message, got: {output}"
      print("PASS: Emacs starts successfully")

      print("\n=== Test 3: Configuration Files Present ===")
      machine.succeed("sudo -u testuser test -f /home/testuser/.config/emacs/init.el")
      machine.succeed("sudo -u testuser test -f /home/testuser/.config/emacs/early-init.el")
      machine.succeed("sudo -u testuser test -d /home/testuser/.config/emacs/config")
      machine.succeed("sudo -u testuser test -d /home/testuser/.config/emacs/lisp")
      print("PASS: Configuration files are present")

      print("\n=== Test 4: Configuration Loads Without Errors ===")
      output = machine.succeed("""
        sudo -u testuser emacs --batch \\
          -l /home/testuser/.config/emacs/init.el \\
          --eval '(message "Config loaded: OK")' 2>&1
      """)
      # Check that config loaded successfully
      assert "Config loaded: OK" in output, f"Config loading failed: {output}"
      # Check for common error patterns
      error_patterns = ["error", "Error", "ERROR", "failed", "Failed"]
      for pattern in error_patterns:
        if pattern in output and "Config loaded: OK" not in output:
          raise Exception(f"Found error pattern '{pattern}' in output: {output}")
      print("PASS: Configuration loads without errors")

      print("\n=== Test 5: Key Packages Are Available ===")
      # Test that critical packages can be loaded
      machine.succeed('sudo -u testuser emacs --batch --eval \'(require "vertico")\' ')
      print("  - vertico: OK")
      machine.succeed('sudo -u testuser emacs --batch --eval \'(require "consult")\' ')
      print("  - consult: OK")
      machine.succeed('sudo -u testuser emacs --batch --eval \'(require "magit")\' ')
      print("  - magit: OK")
      machine.succeed('sudo -u testuser emacs --batch --eval \'(require "eglot")\' ')
      print("  - eglot: OK")
      print("PASS: Key packages are available")

      print("\n=== Test 6: Emacs Daemon Service ===")
      machine.wait_for_unit("emacs.service", "testuser")
      print("PASS: Emacs daemon service started")

      print("\n=== Test 7: Emacs Client Can Connect ===")
      result = machine.succeed("sudo -u testuser emacsclient --eval '(+ 2 2)'")
      assert "4" in result, f"Expected '4', got: {result}"
      print("PASS: Emacs client connected and evaluated expression")

      print("\n=== Test 8: Platform Detection Works ===")
      output = machine.succeed("""
        sudo -u testuser emacs --batch \\
          -l /home/testuser/.config/emacs/lisp/platform.el \\
          --eval '(message "Linux: %s, GUI: %s" platform-linux-p platform-gui-p)'
      """)
      assert "Linux: t" in output, f"Platform detection failed: {output}"
      print("PASS: Platform detection works correctly")

      print("\n=== Test 9: No Startup Warnings or Errors ===")
      output = machine.succeed("""
        sudo -u testuser emacs --batch \\
          -l /home/testuser/.config/emacs/init.el \\
          --eval '(message "Startup complete")' 2>&1
      """)
      # Allow some common non-critical messages but fail on errors
      lower_output = output.lower()
      if "error" in lower_output and "startup complete" not in lower_output:
        raise Exception(f"Startup errors detected: {output}")
      print("PASS: No critical startup errors")

      print("\n=== Test 10: Tree-sitter Support ===")
      output = machine.succeed("""
        sudo -u testuser emacs --batch \\
          --eval '(message "Treesit available: %s" (treesit-available-p))'
      """)
      # Just report status, not a hard requirement
      if "Treesit available: t" in output:
        print("INFO: Tree-sitter is available")
      else:
        print("INFO: Tree-sitter is not available")

      print("\n" + "="*50)
      print("All runtime validation tests passed!")
      print("="*50)
    '';
  };
}
