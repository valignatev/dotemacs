;; -*- lexical-binding: t; -*-

(load (format "%s%s" (file-name-directory user-init-file) "simpc-mode.el"))

(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-h h"))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      create-lockfiles nil
      ;; Starting scratch buffer in fundamental mode instead
      ;; of elisp-mode saves startup time
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      scroll-conservatively 101
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3)
      x-gtk-use-system-tooltips nil
      use-dialog-box nil
      auto-window-vscroll nil
      vc-follow-symlinks t
      confirm-kill-processes nil
      echo-keystrokes 0.5
      dired-dwim-target t
      tab-always-indent t
      ;; 1mb
      read-process-output-max (* 1024 1024)
      column-number-indicator-zero-based nil
      save-interprogram-paste-before-kill t
      truncate-partial-width-windows nil
      ring-bell-function 'ignore
      require-final-newline t
      imenu-max-items 1000
      imenu-max-item-length 1000
      eldoc-echo-area-use-multiline-p nil
      pixel-scroll-precision-interpolate-page t
      make-backup-files nil
      mouse-wheel-tilt-scroll t
      warning-minimum-level :error
      gdb-many-windows t)

(setq-default select-active-regions nil
              indent-tabs-mode nil
              truncate-lines t
              tab-width 4)

(defvar IS-WINDOWS (eq system-type 'windows-nt))

;; Determine OS-specific terminal emulator
(when (equal (getenv "TERM") "dumb")
  (setenv "TERM" (if IS-WINDOWS "msys" (getenv ("TERMINAL")))))
(defvar terminal (if IS-WINDOWS "powershell.exe" (getenv "TERMINAL")))
;; Normal powershell.exe is just a shell, we need to tell it to actually start
;; a gui window with these args
(defvar terminal-args (if IS-WINDOWS "Start-Process PowerShell" nil))

;; Font settings
(defvar font-name "Hack")
(defvar font-size (if IS-WINDOWS 14 17))
(set-frame-font (format "%s-%d" font-name font-size) t t)

(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(global-auto-revert-mode t)
(recentf-mode 1)
(winner-mode 1)
(pixel-scroll-precision-mode t)
(global-hl-line-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

(defun my/edit-init-file ()
  "Opens init.el for editing"
  (interactive)
  (find-file user-init-file))

(defun my/terminal-in-project-root (arg)
  "Opens the $TERMINAL in project root.
With ARG, opens in in the current working directory"
  (interactive "P")
  (let ((default-directory
          (if arg default-directory
            (car (cdr (cdr (project-current)))))))
    (if terminal-args (start-process "terminal" nil terminal terminal-args)
      (start-process "terminal" nil terminal))))

(defun my/copy-file-name ()
  "Copy current buffer's file path to clipboard/kill-ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun my/copy-pwd ()
  "Copy pwd to clipboard/kill-ring."
  (interactive)
  (kill-new default-directory))

;; HIDPI SUPPORT. Mostly quite basic, only tested on Windows so far.
;; Need to Change high DPI settings -> "Override high DPI scaling begavior." ->
;; Scaling performed by: Application for runemacs.exe
(defvar base-dpi 96 "Mininal DPI that would have scale factor equal 1.0")

(defun my/get-current-dpi ()
  "Gets the DPI of the monitor where the frame is placed"
  (let* ((geometry (frame-monitor-attribute 'geometry))
         (mm-size (frame-monitor-attribute 'mm-size))
         (width-res (/ (caddr geometry) 1.0))
         (height-res (/ (cadddr geometry) 1.0))
         (width-mm (car mm-size))
         (height-mm (cadr mm-size)))
    (floor (* 0.5 (+
                   (* (/ width-res width-mm) 25.4)
                   (* (/ height-res height-mm) 25.4))))))

(defun my/get-scale-factor (dpi)
  "Gets scale factor as a floating point number with one ditig after comma
based on the DPI."
  ;; I need to multiply and then divide by 10.0 because floor doesn't support
  ;; flooring presicion, it can only return integers.
  ;; base-dpi * 1.0 instead of just base-dpi to force floating division
  (let ((dpi (if (< dpi base-dpi) base-dpi dpi)))
    (/ (floor (* (/ dpi (* base-dpi 1.0)) 10)) 10.0)))

;; TODO: might need to keep per-frame info incase different frames
;; are on different monitors
(defvar current-frame-dpi (my/get-current-dpi))

(defun my/scale-interface (old-dpi new-dpi)
  "Rescales emacs when I drag frames across monitors.
Support for more interface parts will be added as I feel like it"
  (interactive)
  (let ((scale-factor (my/get-scale-factor new-dpi))
        (old-scale-factor (my/get-scale-factor old-dpi)))
    (run-with-idle-timer 0.3 nil
                         'set-frame-size
                         nil
                         (round (* (/ (frame-text-width) old-scale-factor) scale-factor))
                         (round (* (/ (frame-text-height) old-scale-factor) scale-factor))
                         t)
    (set-frame-font (format "%s-%d" font-name (* font-size scale-factor)) nil nil)
  ))

;; TODO: scale the frame that's just been created
(setq move-frame-functions
      (lambda (frame)
        (let ((old-frame-dpi current-frame-dpi))
          (setq current-frame-dpi (my/get-current-dpi))
          (when (/= old-frame-dpi current-frame-dpi)
            (my/scale-interface old-frame-dpi current-frame-dpi)))))

;; elpaca boilerplate
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; basically ensure elpaca is loaded before continuing
(elpaca-wait)
(elpaca bind-key)
;; (elpaca compat :demand t)

(elpaca use-package :demand t)

(setq use-package-hook-name-suffix nil)
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; load no-littering as early as possible, but not earlier than compat
;; (elpaca no-littering :demand t
;;   (require 'no-littering)
;;   (savehist-mode))

(use-package no-littering
  :config
  (require 'no-littering)
  (savehist-mode))
(elpaca-wait)

(use-package spacemacs-theme
  ; :defer t
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italics t)
  ;; (load-theme 'spacemacs-light t)
  :custom-face
  (font-lock-type-face ((t (:inherit nil))))
  ;; This will unset region background
  ;; (region ((t (:background nil))))
  :hook
  ((dired-mode-hook . dired-hide-details-mode)))

(use-package modus-themes)

(use-package organic-green-theme
  ;; :init (load-theme 'organic-green t)
  :custom
  (organic-green-boldless t)
  (organic-green-version 2))

(use-package ef-themes
  :init (load-theme 'ef-bio);;(load-theme 'ef-spring t)
        (set-face-italic-p 'italic nil)
        (set-face-bold-p 'bold nil)

        (set-face-background 'default "#052525"))

(use-package emacs
  :ensure nil
  :mode (("Pipfile\\'" . conf-toml-mode)
         ("Pipfile.lock\\'" . js-mode)
         ("requirements.txt\\'" . conf-mode)
         ("\\.styl\\'" . css-mode)
         ("\\.py\\'" . python-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.go\\'" . go-ts-mode)
         ("\\.c\\'" . simpc-mode)
         ("\\.h\\'" . simpc-mode)
         ("\\.html\\'" . html-mode)
         ("\\.env\\'" . conf-mode)
         ("\\.env.local\\'" . conf-mode))
  :bind (("C-x t" . my/terminal-in-project-root)
         ("<f7>" . recompile)
         ("S-<wheel-up>" . (lambda () (interactive) (scroll-right 5)))
         ("S-<wheel-down>" . (lambda () (interactive) (scroll-left 5))))
  :hook ((text-mode-hook . visual-line-mode)
         (org-mode-hook . visual-line-mode)
         (compilation-mode-hook . visual-line-mode)
         (python-mode-hook . (lambda() (setq python-indent-def-block-scale 1)))
         (js-mode-hook . (lambda() (setq js-indent-level 2))))
  :config
  (setq js-indent-level 2
        css-indent-offset 2)
  (setq-default
   c-basic-offset 4))

(use-package org
  :defer t
  :config (push 'md org-export-backends))

;; Magic garbage collector hack
;; It's kinda small so maybe makes sense to just copy/paste
;; it into a config instead of installing it with straight
(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024))  ; 100mb
  :hook ((window-setup-hook . gcmh-mode)))

(use-package minions
  :config (minions-mode 1))

(use-package transient)

(use-package magit
  :after (transient)
  :init
  (setq magit-diff-refine-hunk t
        git-commit-summary-max-length 73
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-save-repository-buffers nil)
  :commands (magit-status)
  :bind (("C-x g" . magit-status)))

(use-package git-link
  :init
  (setq git-link-open-in-browser t)
  :commands (git-link git-link-commit)
  :custom (git-link-open-in-browser t))

(use-package evil
  :init
  (setq evil-default-state 'emacs
        evil-want-C-w-in-emacs-state t
        evil-want-C-w-delete nil
        evil-want-Y-yank-to-eol t
        evil-want-C-u-scroll t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo
        evil-symbol-word-search t)
  :config
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'conf-mode 'normal)
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'emacs)
  (defalias #'forward-evil-word #'forward-evil-symbol)
 :hook (elpaca-after-init-hook . evil-mode))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-exchange
  :after evil
  :config (evil-exchange-install))

(use-package vertico
  ;; :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Remove from now to test orderless + savehist
;; plus can't load the package for some reason:
;; https://github.com/radian-software/straight.el/issues/1089
;; (use-package prescient
;;   :config
;;   (use-package vertico-prescient
;;     :after (vertico)
;;     :config
;;     (vertico-prescient-mode 1)
;;     (prescient-persist-mode 1)))


(defvar my/deadgrep-global-path default-directory)
(defun my/deadgrep-project-root-function ()
  my/deadgrep-global-path)

(use-package deadgrep

  :config
  (defun my/deadgrep-vanilla ()
    (interactive)
    (setq deadgrep-extra-arguments '("--no-config"))
    (call-interactively 'deadgrep))

  (defun my/deadgrep-u ()
    (interactive)
    (setq deadgrep-extra-arguments '("--no-config" "-u"))
    (call-interactively 'deadgrep))

  (defun my/deadgrep-global ()
    (interactive)
    (let ((deadgrep-project-root-function #'my/deadgrep-project-root-function))
      (call-interactively 'deadgrep)))

  :bind (("<f5>" . my/deadgrep-vanilla)
         ("<f6>" . my/deadgrep-u)
         ("C-<f5>" . my/deadgrep-global)))

(use-package dockerfile-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package zig-mode
  :defer t)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package php-mode
  :defer t)

(use-package async
  :defer t)

;; (use-package editorconfig
;;   :defer t
;;   :hook ((prog-mode-hook . (lambda () ((editorconfig-mode 1))))))

(use-package editorconfig
  :config (editorconfig-mode 1))

;; Two use-package to silence elpaca complains about required versions
;; I need to figure out version locking idk
;; Needed for eglot
(use-package eldoc
  :ensure t)

;; Needed for eglot
(use-package jsonrpc
  :defer t)

(use-package eglot
  :defer t
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-events-buffer-size 0)
  :config
  (add-to-list
   'eglot-server-programs
   '((js-ts-mode tsx-ts-mode typescript-ts-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '(jai-mode . ("/home/vj/projects/3rdparty/Jails/bin/jails" "-jai_path" "/home/vj/projects/3rdparty/jai/bin/jai"))))

(use-package web-mode
  :init (setq web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2)
  :mode (("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("blade"  . "\\.blade\\.php\\'")))
  :defer t)

(use-package odin-mode
  ;; More syntax support before getting merged
  :ensure (odin-mode :host github :repo "mattt-b/odin-mode")
  :config (setq indent-tabs-mode nil)
  :hook ((odin-mode-hook . (lambda () (setq js-indent-level 4
                                            indent-tabs-mode nil)))))

(use-package go-mode
  :defer t)

(defun setup-jai-mode ()
  (setq js-indent-level 4
        indent-tabs-mode nil
        my/deadgrep-global-path (car (split-string (executable-find "jai") "/bin/jai"))))

(use-package jai-mode
  :defer t
  :ensure (jai-mode :host github :repo "valignatev/jai-mode")
  :config
  (setq compile-command "jai build.jai")
  (font-lock-add-keywords 'jai-mode
                          '(("\\<\\(or_else\\|or_return\\|or_continue\\|or_break\\|push_my_context\\)\\>" . font-lock-keyword-face)))
  :hook ((jai-mode-hook . setup-jai-mode)))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; (use-package visible-mark
;;   :init (setq visible-mark-faces `(visible-mark-active))
;;   :config
;;   (global-visible-mark-mode 1))







;; Courtecy of a kind stranger from jai secret beta discord
;; TODO: tailor it to my needs

(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR.
source: https://stackoverflow.com/questions/24356401/how-to-append-multiple-elements-to-a-list-in-emacs-lisp"
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))
(setq build-script-names (list
                          (if IS-WINDOWS (cons "build.ps1" "powershell.exe -ExecutionPolicy Unrestricted -File build.ps1"))
                          (if IS-WINDOWS "build.bat"         "./build.sh")
                          (if IS-WINDOWS "build-windows.bat" "./build-linux.sh")
                          (if IS-WINDOWS "bob.exe"           "./bob")
                          (cons "bob.c" (let ((target-exe (if IS-WINDOWS "bob.exe" "bob")))
                                          (cond ((executable-find "clang") (concat "clang bob.c -o " target-exe))
                                                ((executable-find "gcc")   (concat "gcc bob.c -o "   target-exe))
                                                ((executable-find "cl")    (concat "cl bob.c /Fe:"   target-exe))
                                                (t (error "no C compiler for bob.c found")))))
                          '("CMakeLists.txt" . "cmake -S . -B __build__ && cmake --build __build__")
                          (if IS-WINDOWS '("build.sh" . "wsl bash -ic ./build.sh"))
                          (if IS-WINDOWS
                              '("Makefile" . "wsl make")
                            '("Makefile"       . "make"))
                          ))
(if IS-WINDOWS
    (append-to-list 'build-script-names
                    (list '("build.jai"      . "jai build.jai -quiet -exe app && app")
                          '("main.jai"       . "jai main.jai -quiet -exe app && app")
                          '("first.jai"      . "jai first.jai -quiet -exe app && app")))
  (append-to-list 'build-script-names
                  (list '("build.jai"      . "time jai build.jai -quiet -exe app && time ./app")
                        '("main.jai"       . "time jai main.jai -quiet -exe app && time ./app")
                        '("first.jai"      . "time jai first.jai -quiet -exe app && time ./app"))))



(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'msbuild-error)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(msbuild-error
                 "^\\(.*?\\)(\\([0-9]+\\),\\([0-9]+\\)): \\(?:\\(fatal \\)?error\\|\\(warning\\)\\|\\(message\\)\\) .*?:" 1 2 3 (4))))

(defun first-non-nil (l)
  (unless (null l)
    (if (car l)
        (car l)
      (first-non-nil (cdr l)))))

(defun filter-non-nil (l)
  (unless (null l)
    (if (car l)
        (cons (car l) (filter-non-nil (cdr l)))
      (filter-non-nil (cdr l)))))

(cl-defun longest-car-string (l &optional (max-so-far (cons nil nil)) (max-so-far-len 0))
  (if l (let* ((element (caar l))
               (others  (cdr l))
               (strlen  (length element)))
          (if (> strlen max-so-far-len)
              (longest-car-string others (car l) strlen)
            (longest-car-string others max-so-far max-so-far-len)))
    max-so-far))

(defun find-closest-build-script ()
  (let* ((potential-paths (mapcar (lambda (build-script-name)
                                    (let ((name (if (consp build-script-name) (car build-script-name) build-script-name)))
                                      (when name ;; NOTE(kind stranger from jai secret beta discord): name could be
                                        ;; nil since this kind of
                                        ;; build system is not
                                        ;; available on this
                                        ;; architecture
                                        (let ((path (locate-dominating-file (expand-file-name default-directory) name)))
                                          (when path
                                            (cons path name))))))
                                  build-script-names))
         (existing-paths (filter-non-nil potential-paths)))

    (if existing-paths
        (longest-car-string existing-paths)
      (cons nil nil))
    ))

(cl-defun translate-build-file-to-command (file &optional (build-scrips build-script-names))
  (unless build-scrips
    (error "unkown build script"))
  (let* ((first          (car build-scrips))
         (iter-file-name (if (consp first) (car first) first)))
    (if (string= file iter-file-name)
        (if (consp first) (cdr first) first)
      (translate-build-file-to-command file (cdr build-scrips)))))

(setq compilation-finish-functions
      (list (lambda (&rest _)
              (compilation-minor-mode 1))))

(defun find-build-script-and-compile ()
  (interactive)
  (let* ((path-and-script (find-closest-build-script))
         (path   (car path-and-script))
         (script (cdr path-and-script)))
    (unless path
      (error "no build files found"))

    (let ((command (translate-build-file-to-command script))
          (curr-dir default-directory))

      (cd path)
      (compilation-start command t)
      (cd curr-dir))))


;; (push '("^Comint \\(finished\\).*"
;;         (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
;;         (1 compilation-info-face))
;;       compilation-mode-font-lock-keywords)

;; (push '("^Comint \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
;;         (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
;;         (1 compilation-error-face)
;;         (2 compilation-error-face nil t))
;;       compilation-mode-font-lock-keywords)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "72b4e4ad13ff8a62e0426cffbe50939736ac1bac09af753b4da1b28ec99186ec"
     "ad5727801b610ff292f7f8833b47fa35ad18d461023d992497f3fcc10a7b164a"
     "120daecf822f5af47ec6c03ea654e722ff86135970a3b5d774f12f329f930bb5"
     "c451656eb8440a62dd2a3b140a8516647d39b912da1262fc1779f8f2bcc63925"
     "acaa346f0baf7d9f6e04bb3cbfce0657817b39db333e612a7a12a34e5f2d04d5"
     "4b59396c6a4cc8ca75c3addb4ee1021af6dc69e2f15955a39f055df28cf3f81e"
     "f92f07aaf462d3b6ff0f054c55a5bde605828bbb66180c24b6ded204e2206863"
     "f90edafccd777cd3df7b88bb5bb72931d8cbd5ef92423fbca0c3bc344e03f1e2"
     "ecc2e41ce0e5b6671dd1f43478b9be35fb0fe811d25d20c46df68472d6f623e6"
     "de4382be014e101c135c3d7c638a488e5cb843fa61554008f08e7bec1bc1abb6"
     "48cfb7c9e5ad83901f9789790a7ae427ab4242005c2ee6d0b0f0b5ff5e3f724e"
     "0924b7ae3f77f1eb29ea6a3bed73539ac4b26e2e60c4fd51ad00196df43005c5"
     "16155a11eea450abb13a82e636bbe9e216966898cecbec7a7328ca25ac2e1767"
     "4861ed05311cce238097dc81a922d28c291c3073512d2a2d9a36dc3855dadc5c"
     "af4e265175af859104e4db52e7b16b95d6bc16c2c48ec43462c68b314795c8bf"
     "f8cbad74cca5b307522e5078709cafeff74ef789e5be469b0956c027aef62fdf"
     "7762a9a98d1c7e1f3eaabeb743f16aa016f39e4e3b20ac40e04fcd7475aae866"
     "6ed2a90b13f77bac6b75f36352d72378b2a17346518884675aa56339a3bc38a9"
     "d5d3915d6a61fc9cd5c8b3d2cce73857fc4d8eb18b944964422341cb78d693e8"
     "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79"
     "1b786d3723357a41cbb42c0a9f7e000e3f8c1c488691c0e92b7c1c69d49bf169"
     "4c33bd6d84f2155c63aca2f49b6564f616030e32bddef1cf41e34ff70a4f3eb2"
     "6b91ddbc12b0fc9f43fb9e31be8b76b16e387312232324c48e8c666418fe643c"
     "d9a947788a4c5f7051c4ad3a3e0e9d76218209899683d3e9ed1e2aa6cd10d462"
     "70b0a307c84ae939e8700fe0b2d45043103f4cd03fde8ed69ccee6b2f164dd43"
     "0279c1b81b569e46a4ee8e87e5e381568bf931257287191fd091f2948f7e2e8e"
     "48bf9307e1468f596789717043cc5a332ce30310cbb747ae0d2ce736ff23538f"
     "bc137acdbe14673fbba58c340a1d530055df1ef1624f83c479364832f2653cb3"
     "9d8d60e69db09062e759f008e59fac1d76618b09cddaf06cef093653c16f1e75"
     "ead62296b48539886f4abbefa54c7a3b294144d62fed60ee3cae08a2b1c1f0cf"
     "661c38fdd381265206a1d74a765ef1a219ed071c26082547aaacbb92064ffbf1"
     "077f3e88cee64127903484d97a54dbecc40f4f6b4b7e266b1023991d2c31d25a"
     "342ed922b461613b79c29624c4bed600466bef9270b0763bb81bcf7c7a011bea"
     "5d05e8db28c566fe2a4e8bd87a17bf89bbaddbdbe76fc15e1f67b1a61f80c621"
     "4edad12267c88bb57aab5a5c0d2e23740c6f552b6a36fb785dfb4e4725808eab"
     "9f986dcc0de26c1c8b6dfd749eb7351b1a3c8db31b3330a7dfdd25be1b47cb22"
     "a32c59022bb35d2f7b56960c2c5b8b07ec5b9a66c2a47de58f19ec74b834862e"
     "372905daccda4574b28e5d5b64d5a4059da9e3c5548abc274af04fe63adc1372"
     "9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2"
     "1594eb8fc081be254c7df7b2a37e9808f79c94863366da6d34bbe735519a30f5"
     "1d6b446390c172036395b3b87b75321cc6af7723c7545b28379b46cc1ae0af9e"
     "e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032"
     "b0cbcb2fa0c69ab36f4505fec9967969b73327f1b8564f9afface8afd216bc93"
     "8d146df8bd640320d5ca94d2913392bc6f763d5bc2bb47bed8e14975017eea91"
     "99d1e29934b9e712651d29735dd8dcd431a651dfbe039df158aa973461af003e"
     "fef6645175d4c5f9d573daca2ba4d7efa781be10967024d1d8e6ef0c4aa71437"
     "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "#CCCCCC" :weight normal))))
 '(font-lock-type-face ((t (:inherit nil))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#119911" :underline t))))
 '(tree-sitter-hl-face:property ((t (:inherit 'font-lock-constant-face))))
 '(visible-mark-active ((t (:background "light salmon")))))
