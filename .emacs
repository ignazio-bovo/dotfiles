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
        ("SPC" . corfu-insert-separator)
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
     (propertize (if (= (user-uid) 0) " #" " $") 'face `(:foreground "magenta"))
     (propertize " " 'face `(:bold t)))
    )

  (setq eshell-prompt-function 'my-eshell-prompt-function)
  )

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

(use-package writeroom-mode)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  ;; :config
  ;;   (set-face-attribute 'org-superstar-header-bullet nil :inherit 'fixed-pitched :height 180)
  :custom
  ;; set the leading bullet to be a space. For alignment purposes I use an em-quad space (U+2001)
  (org-superstar-headline-bullets-list '(""))
  (org-superstar-todo-bullet-alist '(("DONE" . ? )
                                     ("HOLD" . ?❍)
                                     ("TODO" . ?☐)))
  (org-superstar-special-todo-items t)
  (org-superstar-leading-bullet "")
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?•)))
  )

(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  (defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
    "Formats for `format-time-string' which are used for time stamps.")
  (setq is-graphic-env (display-graphic-p)
        org-ellipsis "..."
        org-startup-with-inline-images is-graphic-env
        org-image-actual-width '(300))

  (setq org-agenda-files
        (list "~/Documents/todo.org"))

  (setq org-return-follows-link t)
  (setq org-capture-templates
        '(("t" "Inbox Todo" entry (file "~/Documents/todo.org")
           "* TODO %?" :empty-lines 1)
          ("i" "Issue fix" entry (file "~/Documents/todo.org")
           "* TODO %?\t%^G" :empty-lines 1)
          ("w" "Work Todo" entry (file "~/Documents/todo.org")
           "* TODO %?" :empty-lines 1
          )))

  (defun my-org-config/after-org-mode-load ()
    (visual-line-mode)
    (require 'org-indent)
    (org-indent-mode)
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    (variable-pitch-mode 1)
    (writeroom-mode)

    ;; pretty symbols
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (push '("TODO" . "") prettify-symbols-alist)
    (push '("DONE" . "") prettify-symbols-alist)
    (prettify-symbols-mode)
  )

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "VERIFY(v)" "|" "DONE(d)" "CANCELED(c)")))

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :hook (org-mode . my-org-config/after-org-mode-load)
  )

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

;; haskell
(use-package haskell-mode
  :mode ("//.hs" . haskell-mode))

;; rust
(use-package rustic
  :mode ("//.rs$" . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors))
  :config
  (setq rustic-format-on-save t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure flex
  :config
  (setq lsp-disabled-clients '(rls))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package whitespace
  :config
  (setq whitespace-line-column 120)
  :hook (prog-mode . whitespace-mode))

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

(use-package github-theme
  :config
  (load-theme 'github t))

(use-package doom-modeline
  :custom
  (setq doom-modeline-window-width-limit 0.25)
  (setq doom-modeline-gnus t)
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package emojify
  :hook (org-mode . emojify-mode))

(use-package ligature
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

(menu-bar-mode 0)
(fringe-mode -1)
(if (display-graphic-p)
    (progn
      (setq visible-bell nil)
      (setq ring-bell-function 'ignore)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (add-to-list 'default-frame-alist '(font . "Fira Code-12"))
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

(use-package erc
  :bind (:map erc-mode-map
              ("C-<return>" . newline)))

(use-package eldoc
  :diminish t)

;; agenda at startup
;; (add-hook 'after-init-hook 'org-agenda-list)
;; (setq inhibit-splash-screen t) (org-agenda-list) (delete-other-windows) 
(setq initial-buffer-choice (lambda ()
    (org-agenda-list 1)
    (get-buffer "*Org Agenda*")
    (delete-other-windows)))

(use-package elpher)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ffffff" "#183691" "#969896" "#a71d5d" "#969896" "#969896" "#795da3" "#969896"])
 '(custom-safe-themes
   '("30bede69d6e319b387b521218649933028811c514c8c9743194afd998d96d594" "a0f430cb7b3f6c11f1ce13dc6bc91e620ce7122cea0453852dc40b50ef23673a" "1024c2539fb78332eb5e2a4c2a8608a100fb6f2f54393273a7395cd00ff20cbf" "4786ab1579ac8146387ac96b6593f1a7738352ebebb0318be353a4d5441c1b59" "aee108fc40314f8f635d9801a26b973c87340916ce94c9fa9043f5d2328bcb96" "da10fd02c9b85978972cbd4bdc3c8305527410374e49f8e3a49b25c393999168" "a4d0073576f46a9fa52a35cf6e34168c92a780d463e1c9e72f61627ecbf4697e" "b37c6794006a7080983823f59b6268182c0bce2e978d7325e2b30659952153d5" "b2ab8b76c59d3ddcde9185cf3a909678cd63dd1f91e154fc05c9ba351c86e551" "9056e4d59310cada0cca22abf1488d41f34253843b195c8948602155bda18998" "66a767ce5b910e4b2e78e7da943d5f870864d55a81ae52f845765265c777fb90" "e4986d8b70b0390c2d2a9e8ac6117db013a07bf6b82e243452642aacc7a682d2" "c0f45bfef268ab4d403cc94f2e06efb62d34c96ad66291b439330edfa6de2e81" "bf355fc1976f86dcc3d03e5baffced8998d9089ce45e2c2a1e642ac9fa5167e9" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "bcfc77fcc3e012941eb47d5037f0fac767e23fd2dae039214e5fa856ac8bdfdd" "fe15a6144a4f91dbd136c9d2ca74b1e13f502ac5c0b8a35d7daa2a97d2eabd29" "cfb7f51dcfd7a42080fce44babd015f6148524427dea0d27935876880105a391" "350dc341799fbbb81e59d1e6fff2b2c8772d7000e352a5c070aa4317127eee94" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "ed36f8e30f02520ec09be9d74fe2a49f99ce85a3dfdb3a182ccd5f182909f3ab" "96be1c5bb74fc2ffdfed87e46c87f1492969bf2af1fc96232e35c06b517aecc1" "6a83106c40642da92fd2a469886b4ba754923b8c724b5458e93b2b2f50b1d3a1" "7c95ce72313c502645ac8750625a288465da37ba0e7a289920a7c1970c3f66c4" "a6f4ca5606dbebbe606442cfee8cd5bb71d2a94527d93f85c3db36b8052ed2d1" "7271111bf944a4959ede0ab77fe8602f84108ebb299bdade65b6368f06e27a0e" "990ecb38d5ad65e87a70d823ef69abe2256915ce913e1f97907e6591aa00212e" "4f329ff485aed499b054b39e2b58c8a76f715794d407a557040a1c39d01de97c" "041a59f690ddd909e0d3a4b9807d15dbe78d92dbb88bef208ce2eae6b0ad43fd" "daaddf9d99e3a3eb68886621428fbe17e79cb058d5031643c9368b6a67e979a8" "2c24bbc98245f846b194584f0dd4dc4d609af7d718f86f3211f2ef3bcd70c801" "af8afefb8ad99c363555d29dab16f041570314d0ede52ae3b85649f6b63756fe" "2fa33f91b708dc77eaaf9d217e8b8d89e8df12fdabb1f0a0e5b4d9431eb314a1" "fa86ec7a2a083a4a94b350807bad9005a7951317f0c125e687966625008530de" "35e65487aa3165d8eff1053d7e3b514efe053c6960ffd6d7c2d43f60880737ec" "c8b98cad27cd0d4669cd28fbe9746229056d6e98a227a8c075025987572f7285" "a7d376c41a1ac60a7f12ef2cc7824fa481ced48f9492bd842dec64ad08fa4579" "1ab03612222c8b00a8b1a82080479e8eb558bcfe0d610de71e4e3fdd543bb1a0" "4df03703cc200a2e31d406371b794192fa2773917665493ac2b1c73515546d91" "e0cb12e8cc9532816ad8d5027bcfd159518b82cca80c06fd295c7695b3c46d73" "4c6447ad91b516d3591c05d91c446e9dc38899ef5057750adde3085d0ca1feb9" "8a0793fe7fbc2520f0ec6f7e0b3a77f11d3c2af6110a3f907420fe662d00ab32" "a9e12687239a5c69e4f01e95fd400aa9d90bac06ac6018924c1d62ee0528010f" "601a493ed2cba8f7fdfdfdb5e4bef7e0a67a4e03217375856cbd2032f16bd131" "d79bbbab237b0dbd86796485a1d6fbd02934fbda21f433f6e31c2979ba4b4337" "568dc4fd1b5d5b9f4eb39ad96bf749f2d9b6bb6897e6d2e226c50058fa4ad245" "9cdb55816812928fe562f0a3a6915049dd724ac518a569a2a5bbcfa44c6aa0d4" "aaa3c79c1f3ed4371f1b2a7cf583ffeba4072f4f035ab26a2374769c7b54f61c" "1ee98cb7d2bb5cd7179b943ad674d3a9c2825e0fbff0b7f56da6fff57269bec4" "430572bae93c604c38d120c4a007bba1e49f062c52753f883fc3b79fc39a7b87" "0a121b0104adb08eb028ea473a859bd7ba3488e3e660dbc853de854ad964bf17" "c4214e79d610086b2b4302fb8607e83ef7d5264202baa75dd457e27d9768990e" "5373edc58c096dd8fc505ea39d3ddb4103c894d251ba96c0d7b98386cb7f1d88" "55cf175f7b8e4ab94e976a4eec351004c47e48fbbaad08be82998fa029e1910f" "274f0ccbdd1ba3335d623c1748915b461d50fc03c5e4b69f6c089f1275d842d3" "0255b0a795675e21ae9f09d3e1a0aef286c5846a3585b68983b643c27e02228f" "a8ea6e5fb974b4e8655620eb6b5b32341fe62f2f8f7f4624cf6af5d3a35a4b08" "e613c2ffe0d6c9463d67f37275566ab3c47bdd70114fc3387738a4eb292ea156" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" default))
 '(doom-modeline-bar-width 2)
 '(doom-modeline-height 15)
 '(fci-rule-color "#969896")
 '(nrepl-message-colors
   '("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896"))
 '(pdf-view-midnight-colors '("#969896" . "#f8eec7"))
 '(rustic-ansi-faces
   ["black" "red3" "green3" "yellow3" "magenta2" "magenta3" "cyan3" "white"])
 '(rustic-cargo-check-arguments "")
 '(rustic-default-clippy-arguments "--all-features")
 '(rustic-default-test-arguments "--all-features")
 '(vc-annotate-background "#b0cde7")
 '(vc-annotate-color-map
   '((20 . "#969896")
     (40 . "#183691")
     (60 . "#969896")
     (80 . "#969896")
     (100 . "#969896")
     (120 . "#a71d5d")
     (140 . "#969896")
     (160 . "#969896")
     (180 . "#969896")
     (200 . "#969896")
     (220 . "#63a35c")
     (240 . "#0086b3")
     (260 . "#795da3")
     (280 . "#969896")
     (300 . "#0086b3")
     (320 . "#969896")
     (340 . "#a71d5d")
     (360 . "#969896")))
 '(vc-annotate-very-old-color "#969896")
 '(writeroom-mode-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-major-mode ((t nil)))
 '(doom-modeline-project-dir ((t nil)))
 '(font-lock-comment-face ((t nil)))
 '(rustic-compilation-info ((t (:foreground "green"))))
 '(variable-pitch ((t (:family "Open Sans")))))
