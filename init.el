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
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2)
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
      require-final-newline t)
(setq-default indent-tabs-mode nil
              truncate-lines t)

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
            (cdr (project-current)))))
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
  :defer t
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italics t)
  (load-theme 'spacemacs-light t)
  :custom-face
  (font-lock-type-face ((t (:inherit nil))))
  :hook
  ((dired-mode-hook . dired-hide-details-mode)))

(use-package emacs
  :mode (("Pipfile\\'" . conf-toml-mode)
         ("Pipfile.lock\\'" . js-mode)
         ("requirements.txt\\'" . conf-mode)
         ("\\.styl\\'" . css-mode))
  :bind (("C-x t" . my/terminal-in-project-root))
  :hook ((text-mode-hook . visual-line-mode)
         (org-mode-hok . visual-line-mode))
  :config
  (setq js-indent-level 2
        css-indent-offset 2))

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

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  :hook((tree-sitter-after-on-hook . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :custom-face
  (tree-sitter-hl-face:property ((t (:inherit 'font-lock-constant-face)))))

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

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting nil)
  (when IS-WINDOWS
    (setq lsp-zig-zls-executable "C:\\Users\\vj\\projects\\zls\\zig-out\\bin\\zls.exe"))
  :hook (
         (php-mode-hook . lsp)
         (zig-mode-hook . lsp))
  :commands lsp)

(use-package web-mode
  :init (setq web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2)
  :defer t)

(use-package odin-mode
  ;; More syntax support before getting merged
  :straight (odin-mode :type git :host github :repo "corruptmemory/odin-mode")
  ;; This is horrible, but have to do it because odin-mode uses javascript's mode
  ;; indentation facilities
  :hook ((odin-mode-hook . (lambda () (setq js-indent-level 4)))
         (js-mode-hook . (lambda() (setq js-indent-level 2)))))
