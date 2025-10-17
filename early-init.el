;; -*- lexical-binding: t; -*-

;; Hack to reduce startup flashbang (it doesn't fully solve it, but makes it slightly better)
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(background-color . "#131015") default-frame-alist)
(push '(foreground-color . "#ffffff") default-frame-alist)

(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(setq frame-inhibit-implied-resize t)
