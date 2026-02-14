(setq package-enable-at-startup nil)

;; macOS GUI Emacs doesn't inherit shell PATH; ensure Homebrew binaries are available
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))))

;; native-comp: set LIBRARY_PATH for libgccjit to find gcc libraries
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/15"
           "/opt/homebrew/opt/libgccjit/lib/gcc/15"
           "/opt/homebrew/opt/gcc/lib/gcc/current"
           "/opt/homebrew/Cellar/gcc/15.2.0_1/lib/gcc/current/gcc/aarch64-apple-darwin25/15"
           "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
         ":"))
