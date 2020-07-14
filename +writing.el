;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 Atanas Janackovski

;; Author: Atanas Janackovski <atanas.atanas@gmail.com>

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

;; (require 'init-const)
;; (require 'init-custom)
;; (require 'init-funcs)


;; ===========================================================
;; Writing stuff
;; ===========================================================

;; pandoc setup
(use-package! pandoc-mode
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  :hook
  (markdown-mode . pandoc-mode)
  :bind
  ("C-s-j" . pandoc-jump-to-reference)
  :config
  (setq pandoc-data-dir "/Users/atanas/.emacs.d/site-lisp/pandoc-mode/"
        pandoc-citation-jump-function 'pandoc-open-in-ebib))

;; setup ebib to work with pandoc and more
(use-package! ebib
  :config
  (setq ebib-index-columns
        '(("Entry Key" 24 t)
          ("Author/Editor" 40 t)
          ("Year" 6 t)
          ("Title" 66 t)))
  (setq ebib-preload-bib-files '("/Users/atanas/.pandoc/MyLib.bib" "/Users/atanas/OneDrive - Grand Pacific Health Ltd/GPH/GPH.bib")
        ebib-layout 'index-only
        ebib-file-associations '(("pdf" . "open")
                                 ("ps" . "gv")))
  (setq ebib-citation-commands
        '((latex-mode
           (("cite" "\\cite%<[%A]%>[%A]{%(%K%,)}")
            ("paren" "\\parencite%<[%A]%>[%A]{%(%K%,)}")
            ("foot" "\\footcite%<[%A]%>[%A]{%(%K%,)}")
            ("text" "\\textcite%<[%A]%>[%A]{%(%K%,)}")
            ("smart" "\\smartcite%<[%A]%>[%A]{%(%K%,)}")
            ("super" "\\supercite{%K}")
            ("auto" "\\autocite%<[%A]%>[%A]{%(%K%,)}")
            ("cites" "\\cites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("parens" "\\parencites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("foots" "\\footcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("texts" "\\textcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("smarts" "\\smartcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("supers" "\\supercites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("autos" "\\autoscites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
            ("author" "\\citeauthor%<[%A]%>[%A]{%(%K%,)}")
            ("title" "\\citetitle%<[%A]%>[%A]{%(%K%,)}")
            ("year" "\\citeyear%<[%A]%>[%A][%A]{%K}")
            ("date" "\\citedate%<[%A]%>[%A]{%(%K%,)}")
            ("full" "\\fullcite%<[%A]%>[%A]{%(%K%,)}")))
          (org-mode
           (("ebib" "[[ebib:%K][%D]]")))
          (markdown-mode
           (("text" "@%K%< [%A]%>")
            ("paren" "[%<%A %>%(@%K%; )%<, %A%>]")
            ("year" "[-@%K%< %A%>]")))))
  (setq ivy-re-builders-alist '((t . ivy-prescient-non-fuzzy)))
  :hook
  (ebib-entry-mode . visual-line-mode)
  :bind
  ((:map markdown-mode-map
    ("M-s-b" . ebib-insert-citation)
    ("C-c M-s-b" . ebib))
   (:map org-mode-map
    ("M-s-b" . ebib-insert-citation)
    ("C-c M-s-b" . ebib))
   (:map LaTeX-mode-map
    ("M-s-b" . ebib-insert-citation)
    ("C-c M-s-b" . ebib))))

;; distraction-free writing
(use-package! writeroom-mode
  :config
  ;; make big then make small again
  ;; not working consistently, so removed
  ;; (add-hook 'writeroom-mode-hook #'(lambda () (text-scale-increase 2)))
  ;; (add-hook 'writeroom-mode-disable-hook #'(lambda () (text-scale-decrease 4)))
  (setq writeroom-fullscreen-effect 'maximized
        writeroom-extra-line-spacing 0.8)
  :bind (("C-c C-w C-r" . writeroom-mode)
         :map writeroom-mode-map
         ("C-M-<" . writeroom-decrease-width)
         ("C-M->" . writeroom-increase-width)
         ("C-M-=" . writeroom-adjust-width)))

;; identify slopy prose
(use-package! writegood-mode)

;; use poper english
(setq ispell-dictionary "british")

;; ===========================================================
;; Latex-IDE
;; ===========================================================

;; continous compilation for syncing preview with changes
(use-package! auctex-latexmk)

(use-package! tex
  :ensure auctex
  :config
  ;; Indent items by two spaces.
  (setq LaTeX-item-indent 0)
  (setq TeX-PDF-mode t)
  ;; Generate sync file and sync with C-v
  (eval-after-load
      "tex" '(add-to-list 'TeX-command-list
                          '("latexmk" "latexmk -pdf %t --synctex=1" TeX-run-TeX)))
  (setq latex-run-command "pdflatex")
  (setq LaTeX-command "latex --synctex=1")
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "open")) ; "Skim" "open" "PDF Tools"
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex)
  (setq TeX-auto-save t
        TeX-parse-self t)
  ;; Needed to sync TeX and PDF
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (TeX-source-correlate-mode 1)))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  ;; (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  ;; (setq auto-revert-interval 0.5)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (pdf-view-fit-page-to-window) ))
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (reftex-mode))))

(use-package! company-auctex
  :ensure t
  :init (company-auctex-init))

(use-package! reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

(provide '+writing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +writing.el ends here
