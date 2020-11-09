(use-package! spss
  :load-path "./spss-mode"
  :init
  (require 'spss)
  :config
  (add-hook 'spss-mode-hook #'visual-line-mode)
  (add-hook 'spss-mode-hook #'display-line-numbers-mode)
  (add-hook 'spss-mode-hook #'auto-fill-mode)
)

(use-package! mplus-mode
  :load-path "./mplus-mode"
  :init
  (require 'mplus-mode)
  :config
  (add-hook 'mplus-mode-hook #'visual-line-mode)
  (add-hook 'mplus-mode-hook #'display-line-numbers-mode)
  ;; (add-hook 'mplus-mode-hook #'auto-fill-mode)
  ;; config setup as per repo instructions
  (setq auto-mode-alist (cons '("\\.inp" . mplus-mode) auto-mode-alist))
  (add-hook 'mplus-mode-hook
            (lambda ()
              (require 'auto-complete-config)
              (add-to-list 'ac-dictionary-directories "~/.emacs.d/es-ac-dict")
              (setq-default ac-sources '(ac-source-abbrev
                                         ac-source-dictionary
                                         ac-source-words-in-same-mode-buffers))
              (add-to-list 'ac-modes 'mplus-mode)
              (add-hook 'mplus-hook (lambda () (auto-complete-mode 1))))
            )
  :hook
  (mplus-mode . auto-complete-mode)
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

(use-package! xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  ;; :functions (compilation-filter my-advice-compilation-filter)
  :init
  ;; For shell and interpreters
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled
              (make-local-variable 'font-lock-function)
              (setq font-lock-function #'ignore)))
  )

(use-package! all-the-icons-ivy-rich
  ;; not sure if need to list help* here
  :after (counsel-projectile)
  :init (all-the-icons-ivy-rich-mode 1)
  )

(use-package! ivy-rich
  ;; :after all-the-icons-ivy-rich
  :init (ivy-rich-mode 1))

(use-package! all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

;; colorise colour references
(use-package! rainbow-mode
  ;; :config (rainbow-mode t)
  )

(use-package! ace-window
  :config
  (setq aw-scope 'visible)
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  )


(use-package! zoom
  :hook (doom-first-input . zoom-mode)
  :config
  (defun size-callback ()
    (cond ((< (frame-pixel-width) 1280) '(0.618 . 0.618))
          (t                            '(0.5 . 0.5))))
  (setq zoom-size 'size-callback
        zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu org-mode)
        zoom-ignored-buffer-names '("*doom:scratch*" "*info*" "*helpful variable: argv*")
        zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*")
        zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20))))
  )

(add-hook 'text-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'text-mode-hook #'hl-todo-mode)
(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (with-eval-after-load 'dashboard
;;   (setq dashboard-banner-logo-title "\"If you can't explain it simply, you don't understand it well enough.\""
;;         dashboard-set-footer nil
;;         dashboard-foot-icon nil
;;         dashboard-footer ""
;;         dashboard-items nil
;;         dashboard-set-navigator nil))

(remove-hook 'doom-first-input-hook #'savehist-mode)

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

;; (with-eval-after-load 'hideshow
;;   (setq hs-hide-comments-when-hiding-all t))

(map! :leader
      (:prefix-map ("c" . "hide-show")
       :desc "toggle hiding" "h" #'hs-toggle-hiding
       ))

;; ===========================================================
;; VBA
;; ===========================================================
;;
(use-package! vba-mode)

;; ===========================================================
;; Miscellaneous
;; ===========================================================

(use-package! vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

;; (use-package! flyspell-lazy
;;   :after flyspell
;;   :config
;;   (flyspell-lazy-mode 1))

(after! flyspell
  (require 'flyspell-lazy)
  (flyspell-lazy-mode 1))


;; (add-hook 'text-mode-hook
;;           (lambda (flyspell-mode -1)))
;; (add-hook 'outline-mode-hook
;;           (lambda (flyspell-mode -1)))
;; (add-hook 'prog-mode-hook
;;           (lambda (flyspell-prog-mode -1)))

;; (add-hook 'prog-mode-hook #')
;; (add-hook 'text-mode-hook #')

;; stop asking for mc to confit multiple changes
(setq mc/always-run-for-all t)

;; ;; setup grip-mode
;; (with-eval-after-load 'grip
;;   ;; Path to the grip binary
;;   (setq grip-binary-path "/usr/local/bin/grip")

;;   ;; after every text change
;;   (setq grip-update-after-change nil)

;;   ;; Use embedded webkit to preview
;;   (setq grip-preview-use-webkit t)

;;   (require 'auth-source)
;;   (let ((credential (auth-source-user-and-password "api.github.com")))
;;     (setq grip-github-user (car credential)
;;           grip-github-password (cadr credential))))


;; add custom hl-todos and set colours
(with-eval-after-load 'hl-todo
  (add-to-list 'hl-todo-keyword-faces '("ANCHOR" . "#DAF7A6"))
  ;; (add-to-list 'hl-todo-keyword-faces '("REVIEW" . "#5eff33"))
  (add-to-list 'hl-todo-keyword-faces '("KLUDGE" . "#ff8e33"))
  (add-to-list 'hl-todo-keyword-faces '("DONE" . "#ff3349"))
  (add-to-list 'hl-todo-keyword-faces '("SYNOPSIS" . "#4fd4ff"))
  (setq hl-todo-include-modes '(prog-mode text-mode markdown-mode))
  )

;; NOTE this is not workiing as expected
;; TODO remove this code or correct config
;; add to magit status view
;; (with-eval-after-load 'magit
;;   (magit-todos-mode -1)
;;   )


(use-package! lsp
  :hook
  (python-mode . lsp)
  (ess-r-mode  . lsp)
  (sh-mode     . lsp)
  (js-mode     . lsp)
  (yaml-mode   . lsp)
  (lua-mode    . lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))
  (setq lsp-idle-delay 0.5
        lsp-links-check-internal 0.9
        lsp-prefer-capf t
        lsp-ui-sideline-delay 0.9)
  :commands
  lsp)

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-position 'at-point)
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package! company-lsp
  :commands company-lsp)

(use-package! company-box
  :hook (company-mode . company-box-mode))

;; savehist was maxing cpu
(with-eval-after-load 'savehist-mode
  (setq history-length 10)
  ;; (savehist-mode nil)
  )

;; (with-eval-after-load 'org-roam-mode
;;   (remove-hook 'after-init-hook 'org-roam-mode-hook t)
;;   )

(after! company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

(provide '+custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +custom.el ends here
