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

;; add font locks to markdown
(defface my-markdown-code-face
  '((t (:inherit markdown-inline-code-face)))
  "Used for code words within text e.g., `\newpage`")

;; this is not working, needs more work
(font-lock-add-keywords
 'markdown-mode
 '(
   ("\\\\newpage" . 'my-markdown-code-face)
   ))

(defun my-markdown-compile-pandoc (beg end output-buffer)
  "Compiles markdown with the pandoc program, if available.
Returns its exit code."
  (when (executable-find "pandoc")
    (call-process-region
     beg end shell-file-name nil output-buffer nil shell-command-switch
     "pandoc -f markdown -t html --standalone --toc --toc-depth=2 \
--filter=pandoc-crossref --citeproc --lua-filter pagebreak.lua \
--metadata-file=/Users/atanas/.pandoc/doom.yml \
--csl=/Users/atanas/.pandoc/csl/apa-old-doi-prefix.csl \
--bibliography=/Users/atanas/.pandoc/MyLib.bib")))
(advice-add #'+markdown-compile-pandoc :override #'my-markdown-compile-pandoc)

;; pandoc setup
(use-package! pandoc-mode
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  :hook
  (markdown-mode . pandoc-mode)
  :bind
  ("C-s-j" . pandoc-jump-to-reference)
  :config
  (setq pandoc-data-dir "/Users/atanas/.doom.d/pandoc-mode/"
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
  ;; (setq ivy-re-builders-alist '((t . ivy-prescient-non-fuzzy)))
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

(map! (:localleader
       :map (markdown-mode-map org-mode-map)
       :prefix-map ("r" . "refs")
       "i" 'ebib-insert-citation
       "b" 'ebib
       "j" 'pandoc-jump-to-reference)
      ;; (:localleader
      ;;  :map (RefTex-mode-map LaTex-mode-map tex-mode-map)
      ;;  :prefix-map ("r" . "refs")
      ;;  "i" 'ebib-insert-citation
      ;;  "b" 'ebib)
      )


(use-package! openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "vlc"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
               "preview"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "docx" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         (list (openwith-make-extension-regexp
                '("csv" "xls" "xlsx"))
               "Microsoft Excel"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
               "PDF Expert"
               '(file))
         ))
  (openwith-mode 1)
  )
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

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct
  ;; :config
  ;; (setq flyspell-correct-ivy--result '(save . ""))
  )

;; (use-package! flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-ivy
;;   :bind ("C-;" . flyspell-correct-wrapper)
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; (use-package! flyspell-correct-ivy
;;   :after flyspell-correct)

(after! markdown-mode
  (defun aj/pandoc-flyspell-verify ()
    (save-excursion
      (forward-word -1)
      (not (looking-back "@"))))
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (setq flyspell-generic-check-word-predicate 'aj/pandoc-flyspell-verify)))
  )

;; ===========================================================
;; Latex-IDE
;; ===========================================================

;; continous compilation for syncing preview with changes
(use-package! auctex-latexmk)

(use-package! tex
  ;; :ensure auctex
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
  ;; :ensure t
  :init (company-auctex-init))

(use-package! reftex
  ;; :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

(provide '+writing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +writing.el ends here
