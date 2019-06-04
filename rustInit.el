(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  
  )

(use-package flymake-rust
  :ensure t
  :config
  (flymake-mode 1)
  )

(use-package cargo
  :ensure t
  :config
  (cargo-minor-mode 1)
  (setq cargo-process--enable-rust-backtrace t)
  (setq cargo-process--command-build "build --verbose")
  (setq cargo-process--command-run "run --verbose")
  )

(use-package company-racer
  :ensure t
  :config
  (company-mode 1)
  (unless (getenv "RUST_SRC_PATH")
    (setenv "RUST_SRC_PATH" (expand-file-name ; this path must be absolute
			     "/Users/ianwahbe/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")))
  (add-to-list 'company-backends 'company-racer)
  )
