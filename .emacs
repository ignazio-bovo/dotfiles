
(require 'package) ;; Emacs builtin

;; use streight.el
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; set package.el repositories
(setq package-archives
'(
   ("org" . "https:/mode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; backup path
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; yes or no -> y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(use-package diminish)

(use-package selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :config
  (selectrum-mode +1)
  ;; Completing variable names from `M-:`
  (setq enable-recursive-minibuffers t)
  (setq completion-styles '(orderless))

  ;; Persist history over Emacs restarts
  (savehist-mode)

  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :hook (after-init . (lambda () (ido-mode -1)))
  )

;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
     ;; C-s search in file like swiper
         ("C-s" . consult-line)      
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history))

  ;; Enable automatic preview at point in the *Completions* buffer. 
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")


  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; corfu
(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (corfu-global-mode))

(use-package eshell
  :requires magit
  :config
  (defun shortened-path (path max-len)
    "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len)
                  (cdr components))
        (setq str (concat str (if (= 0 (length (car components)))
                                  "/"
                                (string (elt (car components) 0) ?/)))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

  (defun my-eshell-prompt-function ()
    (concat
     (propertize (shortened-path (eshell/pwd) 20) 'face `(:weight bold))
     (propertize (if (= (user-uid) 0) " #" " λ") 'face `(:foreground "magenta"))
     (propertize " " 'face `(:bold t)))
    )

  (setq eshell-prompt-function 'my-eshell-prompt-function)
  )

(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  (defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
    "Formats for `format-time-string' which are used for time stamps.")
  (setq is-graphic-env (display-graphic-p)
        org-startup-indented is-graphic-env
        org-ellipsis "..."
        org-pretty-entities is-graphic-env
        org-hide-emphasis-markers is-graphic-env
        org-startup-with-inline-images is-graphic-env
        org-image-actual-width '(300))
  
  (setq org-agenda-files
        (list "~/Documents/todo.org"
              "~/Documents/habits.org"
              "~/Documents/projects.org"
              "~/Documents/wip.org"
              "~/Documents/work.org"))

  (setq org-return-follows-link t)
  (setq org-capture-templates
        '(("t" "Inbox Todo" entry (file "~/Documents/todo.org")
           "* TODO %?" :empty-lines 1)
          ("i" "Issue fix" entry (file "~/Documents/wip.org")
           "* TODO %?\t%^G" :empty-lines 1)
          ("w" "Work Todo" entry (file "~/Documents/work.org")
           "* TODO %?" :empty-lines 1
          )))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "VERIFY(v)" "|" "DONE(d)" "CANCELED(c)")))

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  
  :hook (org-mode . (lambda ()
                      (if (display-graphic-p)
                        (push '("[ ]" .  "☐") prettify-symbols-alist)
                        (push '("[X]" . "☑" ) prettify-symbols-alist)
                        (push '("[-]" . "❍" ) prettify-symbols-alist)
                        (prettify-symbols-mode))))
  )

(use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :custom
    (org-superstar-headline-bullets-list '("")))


;; ibuffer 
(use-package ibuffer
  :config
  (add-hook 'ibuffer-mode-hook
        (lambda () (hl-line-mode 1)))

  (require 'ibuf-ext)
;  (add-to-list 'ibuffer-never-show-predicates "^\\*[^iM]")
  
  (setq ibuffer-saved-filter-groups
    (quote (("default"
         ("dired" (mode . dired-mode))
         ("org" (or (mode . org-mode)
                (name . "Org*")))
         ("web" (or (mode . eww-mode) (mode . w3m-mode)))
         ("shell" (or (mode . eshell-mode) (mode . sh-mode)))
         ("elisp" (or (mode . emacs-lisp-mode) (mode . inferior-emacs-lisp-mode)))
         ("Torrent" (mode . transmission-files-mode))
         ("LaTeX" (mode . LaTeX/P))
         ("chat" (mode . erc-mode))
         ("Python" (mode . python-mode))
;         ("EXWM" (mode . exwm-mode))
         ("C/C++" (mode . cc-mode))
         ("Rust" (mode . rustic-mode))
         ("Doc" (mode . doc-view-mode))
         ("Git" (name . "Magit*"))
         ("man" (or (name . "*Man")
                (mode . WoMan-mode)))
         ))))
  (add-hook 'ibuffer-mode-hook
        (lambda ()
          (ibuffer-auto-mode 1)
          (ibuffer-switch-to-saved-filter-groups "default")))
  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Don't ask for confirmation to delete marked buffers
  (setq ibuffer-expert t)

  ; collapse default buffers
  (setq mp/ibuffer-collapsed-groups (list "Dired" "Default"))

  (defun my-ibuffer-recent-buffer (old-ibuffer &rest arguments) ()
         "Open ibuffer with cursor pointed to most recent buffer name"
         (let ((recent-buffer-name (buffer-name)))
           (apply old-ibuffer arguments)
           (ibuffer-jump-to-buffer recent-buffer-name)))
  
  (advice-add #'ibuffer :around #'my-ibuffer-recent-buffer)
  :bind
  (("C-x C-b" . ibuffer))
  )


;; command tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  :bind
  (("C-c C-z" . undo-tree-visualize)))

(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode 1)
  :bind
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  )


(use-package magit
  :bind
  ("C-x g" . magit-status))

;; typescript
(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  :hook ((before-save . tide-format-before-save) (typescript-mode . setup-tide-mode))
  )

;; rust
(use-package rustic
  :mode ("//.rs$" . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors))
  :config
  (setq rustic-format-on-save t)
  :custom
  '(rustic-ansi-faces
    ["black" "yellow1" "cyan1" "magenta1" "green1" "magenta1" "cyan1" "white"]))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
 ; (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
;  (setq lsp-disabled-clients '(rls))
  :hook (lsp-mode . lsp-ui-mode))

;; (use-package exwm
;;   :if (display-graphic-p)
;;   :config
;;   (require 'exwm)
;;   (require 'exwm-config)
;;   (exwm-config-example)
;;   )

(use-package ligature
  :if (display-graphic-p)
  :straight (ligature :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package whitespace
  :config
  (setq whitespace-line-column 120)
  :hook (prog-mode . whitespace-mode))

(use-package unicode-fonts
   :if (display-graphic-p)
   :config
    (unicode-fonts-setup))

(use-package almost-mono-themes
  :config
  (load-theme 'almost-mono-black t))

(use-package org-roam
    :after org
    :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
    :custom
    (org-roam-directory (file-truename "~/Documents/wiki"))
    :config
    (org-roam-setup)
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n r" . org-roam-node-random)
           (:map org-mode-map
                 (("C-c n i" . org-roam-node-insert)
                  ("C-c n o" . org-id-get-create)
                  ("C-c n t" . org-roam-tag-add)
                  ("C-c n a" . org-roam-alias-add)
                  ("C-c n l" . org-roam-buffer-toggle)))))

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Documents/wiki"
                deft-extensions '("md" "org")))

(use-package org-dashboard)

(menu-bar-mode 0)
(fringe-mode -1)
(if (display-graphic-p)
    (progn
      (setq visible-bell nil)
      (setq ring-bell-function 'ignore)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (add-to-list 'default-frame-alist '(font . "Fira Code-11"))
      ))

;; tabs proper behavior
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; mail
(use-package gnus
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnimap "151.236.218.173"
                  (nnimap-inbox "INBOX")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)
                  (nnimap-authenticator login)))))

(use-package shr
  :custom
  (shr-use-fonts nil))

(use-package docker)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ecc077ef834d36aa9839ec7997aad035f4586df7271dd492ec75a3b71f0559b3" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" default))
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "yellow1" :weight bold))))
 '(highlight ((t (:background "blue" :foreground "white"))))
 '(org-verbatim ((t (:foreground "green")))))
