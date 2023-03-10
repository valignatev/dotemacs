;; -*- lexical-binding: t; -*-
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      create-lockfiles nil
      ;; Starting scratch buffer in fundamental mode instead
      ;; of elisp-mode saves startup time
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      scroll-conservatively 101
      ;; mouse-wheel-progressive-speed nil
      ;; mouse-wheel-scroll-amount '(2)
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
      eldoc-echo-area-use-multiline-p nil)
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
(defvar font-size (if IS-WINDOWS 13 12))
(set-frame-font (format "%s-%d" font-name font-size) t t)

(pixel-scroll-precision-mode t)
(column-number-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(global-auto-revert-mode t)
(savehist-mode 1)
(recentf-mode 1)
(winner-mode 1)
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

;; straight.el boilerplate
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-hook-name-suffix nil)
(straight-use-package 'use-package)

(use-package spacemacs-theme
  ; :defer t
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italics t)
  (load-theme 'spacemacs-light t)
  :custom-face
  (font-lock-type-face ((t (:inherit nil))))
  ;; This will unset region background
  ;; (region ((t (:background nil))))
  :hook
  ((dired-mode-hook . dired-hide-details-mode)))

(use-package emacs
  :mode (("Pipfile\\'" . conf-toml-mode)
         ("Pipfile.lock\\'" . js-mode)
         ("requirements.txt\\'" . conf-mode)
         ("\\.styl\\'" . css-mode)
         ("\\.py\\'" . python-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.go\\'" . go-ts-mode))
  :bind (("C-x t" . my/terminal-in-project-root))
  :hook ((text-mode-hook . visual-line-mode)
         (org-mode-hook . visual-line-mode)
         (compilation-mode-hook . visual-line-mode)
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

(use-package no-littering
  :config
  (require 'no-littering))

(use-package minions
  :config (minions-mode 1))

(use-package magit
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
 :hook (after-init-hook . evil-mode))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package prescient
  :config
  (use-package selectrum-prescient
    :after (selectrum)
    :config
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

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

(use-package eglot
  :defer t
  :custom (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :config
  (add-to-list
   'eglot-server-programs
   '((js-ts-mode tsx-ts-mode typescript-ts-mode) . ("typescript-language-server" "--stdio"))))

(use-package web-mode
  :init (setq web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2)
  :defer t)

(use-package odin-mode
  ;; More syntax support before getting merged
  :straight (odin-mode :type git :host github :repo "mattt-b/odin-mode")
  :config (setq indent-tabs-mode nil)
  :hook ((odin-mode-hook . (lambda () (setq js-indent-level 4
                                            indent-tabs-mode nil)))))

(use-package go-mode
  :defer t)

(defun setup-jai-mode ()
  (setq js-indent-level 4
        indent-tabs-mode nil)
  (add-to-list 'eglot-server-programs
               '((jai-mode :language-id "jai") . ("/home/vj/projects/3rdparty/jai_lsp/jai_lsp" "-build_file" "main.jai" "-log_level" "1")))
  )

(use-package jai-mode
  :defer t
  :straight (jai-mode :type git :host github :repo "krig/jai-mode")
  :hook ((jai-mode-hook . setup-jai-mode)))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; (defface visible-mark-active ;; put this before (require 'visible-mark)
;;            '((((type tty) (class mono)))
;;              (t (:background "magenta"))) "")
(defface visible-mark-active
  '((((type tty) (class mono)))
    (t (:background "light salmon"))) "")

(use-package visible-mark
  :init (setq visible-mark-faces `(visible-mark-face1))
  :config
  (global-visible-mark-mode 1))







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
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-type-face ((t (:inherit nil))))
 '(tree-sitter-hl-face:property ((t (:inherit 'font-lock-constant-face)))))
