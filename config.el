;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Atanas Janackovski"
      user-mail-address "aj347@uowmail.edu.au"
      ;; lsp-ui-sideline-enable nil
      ;; lsp-enable-symbol-highlighting nil
      )

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))

(setq
 doom-font (font-spec :family "SF Mono Powerline" :size 13)
 doom-big-font (font-spec :family "SF Mono Powerline":size 30)
 doom-variable-pitch-font (font-spec :family "Avenir Next LT Pro" :size 12)
 )

;; There are two ways to load a theme Both assume the theme is installed and
;; available You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; (setq doom-themes-treemacs-theme "Default")
(setq doom-themes-treemacs-theme "doom-colors")

(setq +doom-dashboard-banner-dir
      (concat doom-private-dir "banners/")
      +doom-dashboard-banner-padding '(4 . 10))

;; If you intend to use org, it is recommended you change this!
(setq org-directory "/Users/atanas/Dropbox/Org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'visual)

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)

(setq ns-right-alternate-modifier 'none)
(setq ns-function-modifier 'hyper)

(load! "+ess")
(load! "+writing")
(load! "+custom")
;; (load! "+mail")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(package-selected-packages '(company-auctex auctex polymode ess-view ess))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
