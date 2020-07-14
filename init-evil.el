;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 Atanas Janackovski

;; Author: Atanas Janackovski <atanas.atanas@gmail.com>
;; URL: TODO insert adddress here

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Emacs Package configurations template.
;;

;;; Code:

;; Evil packages
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (progn
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-w-in-emacs-state t)
    ;; (setq evil-search-module        'isearch)
    (setq evil-magic                'very-magic)
    (setq evil-emacs-state-cursor   '("#51afef" bar))
    (setq evil-normal-state-cursor  '("#bbc2cf" box))
    ;; (setq evil-insert-state-cursor  '("#ECBE7B" bar))
    (setq evil-replace-state-cursor '("#cc9393" hbar))
    (setq evil-visual-state-cursor  '("#51afef" hollow))
    (setq evil-want-fine-undo t)
    (setq evil-want-change-word-to-end t)
    (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
      (define-key evil-insert-state-map (kbd "C-n") nil)
      (define-key evil-insert-state-map (kbd "C-p") nil)
      (define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions))
    (evil-ex-define-cmd "q" nil)
    (evil-ex-define-cmd "wq" nil)
    (evil-set-initial-state 'shell-mode 'emacs)
    (evil-set-initial-state 'diff-mode 'emacs)
    (evil-set-initial-state 'inferior-ess-r-mode 'emacs)
    (evil-set-initial-state 'magit-status 'emacs)
    (evil-set-initial-state 'treemacs-mode 'motion)
    )
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-mc
  :after evil
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)
  )

(use-package evil-multiedit
  :after evil
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match)
  :init
  (defvar evil-multiedit-key-map (make-sparse-keymap))
  )

(use-package evil-tutor)

(use-package evil-avy
  :after evil
  )

(use-package evil-smartparens
  :after evil
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package general
  :after evil
  )

(use-package evil-commentary
  :after evil
  )

(use-package evil-nerd-commenter
  :after evil)

(use-package evil-magit
  :after evil
  :init (setq evil-want-keybinding nil))


(use-package evil-goggles
  :after evil
  :demand t
  :config (progn
            (evil-goggles-mode)
            (evil-goggles-use-diff-faces)))

(use-package evil-markdown
  :load-path "plugins/evil-markdown"
  :after '(markdown-mode evil)
  )

(use-package treemacs-evil
  :after '(treemacs evil)
  )

(use-package key-chord
  :demand t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
  )

(general-evil-setup)

(defun aj/evil-insert-delete-back-word ()
  "Made specifically for insert state but works in other states. Deletes
backwards until a space. Not a true vim word or WORD."
  (interactive)
  (delete-region
   (point)
   (save-excursion (skip-syntax-backward "^ ") (point))))

(general-define-key :states '(insert emacs normal)
  (general-chord "uu") 'aj/evil-insert-delete-back-word)

;; more complex example
(general-create-definer evil-centaur-def
  :states '(normal insert emacs visual)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'evil-centaur-prefix-command
  :prefix-map 'evil-centaur-prefix-map)

;; evil basic keymap
(evil-centaur-def
  "/"   'swiper
  ;; comments
  "c"   '(:wk "comment")
  "ci"  'evilnc-comment-or-uncomment-lines
  "cl"  'evilnc-quick-comment-or-uncomment-to-the-line
  "cc"  'evilnc-copy-and-comment-lines
  "cp"  'evilnc-comment-or-uncomment-paragraphs
  "cr"  'comment-or-uncomment-region
  "cv"  'evilnc-toggle-invert-comment-line-by-line
  ;; my personal functions
  "a"   '(:wk "atanas")
  "a'"  '(aj/iterm-focus :which-key "iterm")
  "a?"  '(aj/iterm-goto-filedir-or-home :which-key "iterm - go to dir")
  "aw"  '(aj/pandoc-wc :wk "pandoc wordcount buffer")
  "ao"  'aj/open-in-external-app
  ;; buffer
  "b"   '(:wk "buffer")
  "bd"  'kill-this-buffer
  "br"  'revert-buffer
  ;; projectile
  "p"   '(projectile-command-map :wk "projectile")
  "q"   'delete-window
  "G"   'magit-status
  ";"   '(:wk "eval")
  "; ;" 'eval-last-sexp
  "; :" 'align-regexp
  "; %" 'replace-regexp
  "y"   'counsel-yank-pop
  "."   'counsel-imenu
  ","   'ivy-switch-buffer
  "S"   'save-buffer
  "O"   'ace-window
  )

;; ===========================================================
;; ESS Evil keybindings
;; ===========================================================

;; :major-modes
(general-define-key
 :keymaps 'ess-r-mode-map
 :major-modes t
 :states '(normal insert emacs visual)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 ;; noweb
 "c"     '(:ignore t :which-key "R-noweb")
 "cC"    'ess-eval-chunk-and-go
 "cc"    'ess-eval-chunk
 "cd"    'ess-eval-chunk-and-step
 "cm"    'ess-noweb-mark-chunk
 "cN"    'ess-noweb-previous-chunk
 "cn"    'ess-noweb-next-chunk
 ;; REPL
 "s"     '(:ignore t :wk "R-repl")
 "sB"    'ess-eval-buffer-and-go
 "sb"    'ess-eval-buffer
 "e"     'ess-eval-paragraph-and-step
 "f"     'ess-eval-function
 "i"     'ess-interrupt
 "o"     'ess-eval-word
 "R"     'ess-eval-region
 "sp"    'ess-eval-paragraph-and-step
 "sd"    'ess-eval-region-or-line-and-step
 "sl"    'ess-eval-line
 "sr"    'ess-eval-region
 "st"    'ess-eval-function
 "sw"    'ess-set-working-directory
 ;; R data viewers
 "v"     'ess-view-inspect-df
 "d"     '(:ignore t :wk "R-devtools")
 "di"    'ess-r-devtools-install-package
 "dt"    'ess-r-devtools-test-package
 "dl"    'ess-r-devtools-load-package
 "dc"    'ess-r-devtools-check-package
 "dd"    'ess-r-devtools-document-package
 "df"    'ess-roxy-update-entry
 "ds"    'ess-cent-devtools-setup
 ;; R help
 "h"     '(:ignore t :wk "R-help")
 "ho"    'ess-display-help-on-object
 "hi"    'ess-display-index
 "ha"    'ess-display-help-apropos
 ;; Graphics devices
 "g"     '(:ignore t :wk "R-graphics")
 "gn"    'ess-cent-new-gdev
 "gc"    'ess-cent-cur-gdev
 "gs"    'ess-cent-switch-to-gdev
 "gl"    'ess-cent-list-all-gdev
 "gp"    'ess-cent-save-gdev-pdf
 "gc"    'ess-cent-capture-gdev
 "gj"    'ess-cent-switch-next-gdev
 "gk"    'ess-cent-switch-prev-gdev
 ;; R Markdown
 "rc"    'ess-cent-r-insert-chunk
 ;; Shiny
 "Sr"    'ess-cent-shiny-run-app
 )

;; (general-define-key
;;  :states '(insert emacs normal visual)
;;  :keymaps ess-r-mode-map
;;  :major-modes t
;;  (general-chord "df") 'ess-view-inspect-df)

;; markdown mode
(general-define-key
 :keymaps 'markdown-mode-map
 :major-modes t
 :states '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "b"  '(:ignore t :wk "bib")
 "bc" 'ebib-insert-citation
 "bb" 'ebib
 "bz" 'aj/zotero-cayw
 "m"  '(:ignore t :which-key "markdown")
 "ms" '(:keymap markdown-mode-style-map :which-key "style")
 "mc" '(:keymap markdown-mode-command-map :which-key "command")
 )

;; ** Evil idit

;; (general-define-key
;;  :states '(normal visual insert)
;;  :prefix "M"
;;  "d" 'evil-multiedit-match-and-next)
;; ;; Highlights all matches of the selection in the buffer.

;; (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; ;; incrementally add the next unmatched match.
;; (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; ;; Match selected region.
;; (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; ;; Insert marker at point
;; (define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

;; ;; Same as M-d but in reverse.
;; (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
;; (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

;; ;; OPTIONAL: If you prefer to grab symbols rather than words, use
;; ;; `evil-multiedit-match-symbol-and-next` (or prev).

;; ;; Restore the last group of multiedit regions.
;; (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

;; ;; RET will toggle the region under the cursor
;; (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; ;; ...and in visual mode, RET will disable all fields outside the selected region
;; (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; ;; For moving between edit regions
;; (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
;; (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
;; (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
;; (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

;; ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
;; (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

;; (evil-set-initial-state 'flycheck-error-list-mode 'normal)

(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
