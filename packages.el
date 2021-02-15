;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
                                        ;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)
(unpin! flyspell-correct)
(package! flyspell-correct
  :recipe (:host github
           :repo "d12frosted/flyspell-correct"))
(unpin! flyspell-correct-ivy)
(package! flyspell-correct-ivy
  :recipe (:host github
           :repo "d12frosted/flyspell-correct"
           :files ("flyspell-correct-ivy.el")))
(package! flyspell-lazy)
(package! writegood-mode)
(package! xterm-color)
(package! yaml-mode)
(package! polymode)
(package! poly-markdown)
(package! poly-R)
(package! ess-view)
(package! company-box)
(package! pandoc-mode)
(package! ebib)
(package! auctex)
(package! auctex-latexmk)
(package! company-auctex)
(package! all-the-icons-ivy-rich)
(package! all-the-icons-ibuffer)
(package! rainbow-mode)
(package! openwith)
(package! auto-complete)
(package! vba-mode
  :recipe (:host github
           :repo "ayanyan/vba-mode"))
(package! vimrc-mode)
(package! zoom)
(package! lsp-lua-emmy
  :recipe (:host github
           :repo "phenix3443/lsp-lua-emmy"))
(package! all-the-icons-dired
  :recipe (:host github
           :repo "jtbm37/all-the-icons-dired"))
;; (package! mu4e-alert)
(package! evil-terminal-cursor-changer
  :recipe (:host github
           :repo "7696122/evil-terminal-cursor-changer"))
(package! fennel-mode
  :recipe (:host gitlab
           :repo "technomancy/fennel-mode"))
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))
