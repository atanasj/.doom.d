(use-package! spss
  :load-path "./spss-mode"
  :init
  (require 'spss)
  :config
  (add-hook 'spss-mode-hook #'visual-line-mode)
  (add-hook 'spss-mode-hook #'display-line-numbers-mode)
  (add-hook 'spss-mode-hook #'auto-fill-mode)
)

;; ===========================================================
;; Python
;; ===========================================================

;; (use-package! elpy
;;   :after elpy
;;   :ensure t
;;   ;; :defer t
;;   :init
;;   (defun enable-elpy-once ()
;;     (elpy-enable)
;;     (advice-remove 'python-mode 'enable-elpy-once))
;;   (advice-add 'python-mode :before 'enable-elpy-once)
;;   :config
;;   ;; enable jupyter
;;   (setq python-shell-interpreter "jupyter"
;;         python-shell-interpreter-args "console --simple-prompt"
;;         python-shell-prompt-detect-failure-warning nil
;;         elpy-rpc-python-command "/usr/local/bin/python3")
;;   (add-to-list 'python-shell-completion-native-disabled-interpreters
;;                "jupyter")
;;   ;; enable flycheck
;;   (when (require 'flycheck nil t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )
;; ;; Enable autopep8
;; (use-package! py-autopep8
;;   :after python elpy
;;   :config
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;;   )

;; ===========================================================
;; Visual-Changes
;; ===========================================================

(use-package! all-the-icons-ivy-rich
  ;; not sure if need to list help* here
  :after (counsel prescient)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package! ivy-rich
  :after all-the-icons-ivy-rich
  :init (ivy-rich-mode 1))

(use-package! all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))
;; colorise colour references
(use-package! rainbow-mode
  :config (rainbow-mode t))

;; (add-hook 'text-mode-hook #'visual-line-mode)
;; (add-hook 'text-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'visual-line-mode)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'auto-fill-mode)
;; (add-hook 'occur-mode-hook #'visual-line-mode)

;; ;; the hs-mode made things too stuttery in rmd mode
;; (add-hook 'markdown-mode-hook #'hs-minor-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; (setq-default display-line-numbers-width 3)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;; (with-eval-after-load 'dashboard
;;   (setq dashboard-banner-logo-title "\"If you can't explain it simply, you don't understand it well enough.\""
;;         dashboard-set-footer nil
;;         dashboard-foot-icon nil
;;         dashboard-footer ""
;;         dashboard-items nil
;;         dashboard-set-navigator nil))

;; ===========================================================
;; aj/defun
;; ===========================================================

;; from https://www.emacswiki.org/emacs/BackwardKillLine kill line backwards
(defun aj/backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(global-set-key "\C-cu" 'aj/backward-kill-line) ;; `C-c u'

;; iTerm2 intergration
;; from https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;; return the directory of the file currently opened. If it is a *scratch* buffer or something like that, it simply returns the home directory
(defun aj/get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        "~/"
      (file-name-directory filename))))

;; cd to the directory of the file I am editing in emacs. If I am in a *scratch* buffer or something like that, it cd to the $HOME directory. It then focus the iTerm2 app.
(defun aj/iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n" (aj/get-file-dir-or-home))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )
;; focus the iTerm2 app, without modifying the working directory
(defun aj/iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(global-set-key (kbd "C-c t h") 'aj/iterm-goto-filedir-or-home)
(global-set-key (kbd "C-c t n") 'aj/iterm-focus)

(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq flyspell-generic-check-word-predicate 'aj/pandoc-flyspell-verify)))

(defun aj/pandoc-flyspell-verify ()
  (save-excursion
    (forward-word -1)
    (not (looking-back "@"))))

;; pandoc wordcount
(defun aj/pandoc-wc ()
  "Return wordcount of current buffer using pandoc wordcount.lua"
  (interactive) (shell-command (concat "pandoc --lua-filter wordcount.lua " buffer-file-name)))
;; this might be problematic, as really I only want this for markdown docs

;; preview markdownf file in marked2.app
(defun aj/markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s" (shell-quote-argument (buffer-file-name)))))

(global-set-key "\C-cm" 'aj/markdown-preview-file)

;; with courtesy from https://emacs.stackexchange.com/questions/54939/how-do-i-get-request-el-to-post-the-contents-into-the-buffer/54950?noredirect=1#comment85788_54950
(defun aj/zotero-cayw ()
  "Insert Zotero Cite at point."
  (interactive)
  (insert
   (shell-command-to-string
    "curl -s http://localhost:23119/better-bibtex/cayw?format=pandoc"))
  (do-applescript "tell application \"Emacs\" to activate"))

(global-set-key (kbd "C-s-y") 'aj/zotero-cayw)

;; ===========================================================
;; Keybindings
;; ===========================================================

;; (with-eval-after-load 'undo-tree
;;   ;; make undotree work as expected
;;   (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)
;;   (define-key undo-tree-map (kbd "C-_") nil)
;;   (define-key undo-tree-map (kbd "C-?") 'undo-tree-redo)
;;   (define-key undo-tree-map (kbd "M-_") nil)
;;   (define-key undo-tree-map (kbd "C-z") 'undo-tree-undo)
;;   (define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo))

;; (with-eval-after-load 'drag-stuff
;;   ;; make drag-stuff less annoying
;;   (define-key drag-stuff-mode-map (kbd "<M-left>") nil)
;;   (define-key drag-stuff-mode-map (kbd "<M-right>") nil))

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-mark)
  (define-key ivy-minibuffer-map (kbd "C-S-SPC") 'ivy-unmark))

;; (with-eval-after-load 'company
;;   (define-key company-mode-map (kbd "C-M-s-/") 'company-files))

;; ===========================================================
;; Miscellaneous
;; ===========================================================

(use-package! flyspell-lazy
  :after flyspell
  :config
  (flyspell-lazy-mode 1))

;; (add-hook 'text-mode-hook
;;           (lambda (flyspell-mode -1)))
;; (add-hook 'outline-mode-hook
;;           (lambda (flyspell-mode -1)))
;; (add-hook 'prog-mode-hook
;;           (lambda (flyspell-prog-mode -1)))

;; (add-hook 'prog-mode-hook #'wucuo-start)
;; (add-hook 'text-mode-hook #'wucuo-start)

;; stop asking for mc to confit multiple changes
(setq mc/always-run-for-all t)

;; setup grip-mode
(with-eval-after-load 'grip
  ;; Path to the grip binary
  (setq grip-binary-path "/usr/local/bin/grip")

  ;; after every text change
  (setq grip-update-after-change nil)

  ;; Use embedded webkit to preview
  (setq grip-preview-use-webkit t)

  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))


;; add custom hl-todos and set colours
(with-eval-after-load 'hl-todo
  (add-to-list 'hl-todo-keyword-faces '("ANCHOR" . "#DAF7A6"))
  ;; (add-to-list 'hl-todo-keyword-faces '("REVIEW" . "#5eff33"))
  ;; (add-to-list 'hl-todo-keyword-faces '("NOTE" . "#ff8e33"))
  ;; (add-to-list 'hl-todo-keyword-faces '("TODO" . "#ff3349"))
  (add-to-list 'hl-todo-keyword-faces '("SYNOPSIS" . "#4fd4ff"))
  (setq hl-todo-include-modes '(prog-mode text-mode markdown-mode))
  )

;; NOTE this is not workiing as expected
;; TODO remove this code or correct config
;; add to magit status view
;; (with-eval-after-load 'magit
;;   (magit-todos-mode -1)
;;   )

;; this works well for small screens
(with-eval-after-load 'lsp-ui-mode
  (setq lsp-ui-doc-position 'top))

(use-package! lsp-mode
  :hook prog-mode
  :config
  ;; this makes lsp-mode work much snappier
  (setq lsp-idle-delay 0.5
        lsp-links-check-internal 0.9
        lsp-prefer-capf t
        lsp-ui-sideline-delay 0.9)
  )

;; savehist was maxing cpu
(with-eval-after-load 'savehist-mode
  (setq history-length 10)
  ;; (savehist-mode nil)
  )

;; (with-eval-after-load 'org-roam-mode
;;   (remove-hook 'after-init-hook 'org-roam-mode-hook t)
;;   )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(provide 'init-custom-post)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom-post.el ends here
