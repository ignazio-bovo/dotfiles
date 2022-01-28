
:tanat

"27.1"

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("org-elpa" ("2022-01-28 13:35:37" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2022-01-28 13:35:39" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2022-01-28 13:35:39" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "el-get" ("2022-01-28 13:35:39" nil (:type git :host github :repo "dimitri/el-get" :build nil :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2022-01-28 13:35:39" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2022-01-28 13:35:39" ("emacs") (:type git :host github :repo "raxod502/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "nano-emacs" ("2021-12-10 20:19:23" nil (:type git :host github :repo "rougier/nano-emacs" :package "nano-emacs" :local-repo "nano-emacs")) "nano-theme" ("2021-12-10 21:11:49" ("emacs") (:type git :host github :repo "rougier/nano-theme" :files ("*" (:exclude ".git")) :package "nano-theme" :local-repo "nano-theme")) "use-package" ("2022-01-28 13:35:39" ("emacs" "bind-key") (:type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package" :package "use-package" :local-repo "use-package")) "bind-key" ("2022-01-28 13:35:39" nil (:flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :package "bind-key" :local-repo "use-package" :type git :repo "jwiegley/use-package" :host github)) "diminish" ("2022-01-28 13:35:41" ("emacs") (:type git :flavor melpa :host github :repo "myrjola/diminish.el" :package "diminish" :local-repo "diminish.el")) "counsel" ("2021-12-11 11:05:39" ("emacs" "ivy" "swiper") (:type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper" :package "counsel" :local-repo "swiper")) "ivy" ("2021-12-11 11:05:39" ("emacs") (:flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "doc/ivy-help.org" "ivy-pkg.el") :package "ivy" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "swiper" ("2021-12-11 11:05:39" ("emacs" "ivy") (:flavor melpa :files ("swiper.el" "swiper-pkg.el") :package "swiper" :local-repo "swiper" :type git :repo "abo-abo/swiper" :host github)) "org" ("2022-01-28 13:35:41" ("emacs") (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")) :package "org")) "org-gcal" ("2022-01-08 11:00:31" ("request" "request-deferred" "alert" "persist" "emacs") (:type git :flavor melpa :host github :repo "kidd/org-gcal.el" :package "org-gcal" :local-repo "org-gcal.el")) "request" ("2022-01-28 13:35:48" ("emacs") (:type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request" :package "request" :local-repo "emacs-request")) "request-deferred" ("2022-01-08 11:00:31" ("emacs" "deferred" "request") (:flavor melpa :files ("request-deferred.el" "request-deferred-pkg.el") :package "request-deferred" :local-repo "emacs-request" :type git :repo "tkf/emacs-request" :host github)) "deferred" ("2022-01-08 11:00:31" ("emacs") (:type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred" :package "deferred" :local-repo "emacs-deferred")) "alert" ("2022-01-08 11:00:31" ("gntp" "log4e" "cl-lib") (:type git :flavor melpa :host github :repo "jwiegley/alert" :package "alert" :local-repo "alert")) "gntp" ("2022-01-08 11:00:31" nil (:type git :flavor melpa :host github :repo "tekai/gntp.el" :package "gntp" :local-repo "gntp.el")) "log4e" ("2022-01-08 11:00:31" nil (:type git :flavor melpa :host github :repo "aki2o/log4e" :package "log4e" :local-repo "log4e")) "persist" ("2022-01-08 11:00:31" nil (:type git :host github :repo "emacs-straight/persist" :files ("*" (:exclude ".git")) :package "persist" :local-repo "persist")) "undo-tree" ("2022-01-28 13:35:41" nil (:type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git")) :package "undo-tree" :local-repo "undo-tree")) "guide-key" ("2021-12-11 10:58:20" ("dash" "popwin" "s") (:type git :flavor melpa :host github :repo "kai2nenobu/guide-key" :package "guide-key" :local-repo "guide-key")) "dash" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "popwin" ("2021-12-11 10:58:20" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/popwin" :package "popwin" :local-repo "popwin")) "s" ("2022-01-28 13:35:42" nil (:type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "projectile" ("2022-01-28 13:35:41" ("emacs") (:type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile" :package "projectile" :local-repo "projectile")) "key-chord" ("2021-12-11 10:58:20" ("emacs") (:type git :flavor melpa :host github :repo "emacsorphanage/key-chord" :package "key-chord" :local-repo "key-chord")) "company" ("2021-12-11 10:58:20" ("emacs") (:type git :flavor melpa :files (:defaults "icons" "company-pkg.el") :host github :repo "company-mode/company-mode" :package "company" :local-repo "company-mode")) "company-c-headers" ("2021-12-11 10:58:20" ("emacs" "company") (:type git :flavor melpa :host github :repo "randomphrase/company-c-headers" :package "company-c-headers" :local-repo "company-c-headers")) "irony" ("2022-01-11 05:15:39" ("cl-lib" "json") (:type git :flavor melpa :files ("*.el" "server" "irony-pkg.el") :host github :repo "Sarcasm/irony-mode" :package "irony" :local-repo "irony-mode")) "magit" ("2022-01-28 13:35:42" ("emacs" "dash" "git-commit" "magit-section" "transient" "with-editor") (:type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit" :package "magit" :local-repo "magit")) "git-commit" ("2022-01-28 13:35:42" ("emacs" "dash" "transient" "with-editor") (:flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :package "git-commit" :local-repo "magit" :type git :repo "magit/magit" :host github)) "transient" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient" :package "transient" :local-repo "transient")) "with-editor" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :host github :repo "magit/with-editor" :package "with-editor" :local-repo "with-editor")) "magit-section" ("2022-01-28 13:35:42" ("emacs" "dash") (:flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "Documentation/magit-section.texi" "magit-section-pkg.el") :package "magit-section" :local-repo "magit" :type git :repo "magit/magit" :host github)) "beacon" ("2021-12-11 10:58:22" ("seq") (:type git :flavor melpa :host github :repo "Malabarba/beacon" :package "beacon" :local-repo "beacon")) "smartparens" ("2021-12-11 10:58:26" ("dash" "cl-lib") (:type git :flavor melpa :host github :repo "Fuco1/smartparens" :package "smartparens" :local-repo "smartparens")) "auctex" ("2021-12-11 10:58:32" ("emacs") (:type git :host github :repo "emacs-straight/auctex" :files ("*" (:exclude ".git")) :package "auctex" :local-repo "auctex")) "ace-window" ("2021-12-11 10:58:34" ("avy") (:type git :flavor melpa :host github :repo "abo-abo/ace-window" :package "ace-window" :local-repo "ace-window")) "avy" ("2021-12-11 10:58:34" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "multiple-cursors" ("2021-12-11 10:58:36" ("cl-lib") (:type git :flavor melpa :host github :repo "magnars/multiple-cursors.el" :package "multiple-cursors" :local-repo "multiple-cursors.el")) "flycheck" ("2022-01-28 13:35:42" ("dash" "pkg-info" "let-alist" "seq" "emacs") (:type git :flavor melpa :host github :repo "flycheck/flycheck" :package "flycheck" :local-repo "flycheck")) "pkg-info" ("2022-01-28 13:35:42" ("epl") (:type git :flavor melpa :host github :repo "emacsorphanage/pkg-info" :package "pkg-info" :local-repo "pkg-info")) "epl" ("2022-01-28 13:35:42" ("cl-lib") (:type git :flavor melpa :host github :repo "cask/epl" :package "epl" :local-repo "epl")) "let-alist" ("2022-01-28 13:35:42" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "flycheck-irony" ("2022-01-11 05:15:39" ("emacs" "flycheck" "irony") (:type git :flavor melpa :host github :repo "Sarcasm/flycheck-irony" :package "flycheck-irony" :local-repo "flycheck-irony")) "tide" ("2022-01-28 13:35:42" ("emacs" "dash" "s" "flycheck" "typescript-mode" "cl-lib") (:type git :flavor melpa :files (:defaults "tsserver" "tide-pkg.el") :host github :repo "ananthakumaran/tide" :package "tide" :local-repo "tide")) "typescript-mode" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :host github :repo "emacs-typescript/typescript.el" :package "typescript-mode" :local-repo "typescript.el")) "rustic" ("2022-01-28 13:35:42" ("emacs" "rust-mode" "dash" "f" "let-alist" "markdown-mode" "project" "s" "seq" "spinner" "xterm-color") (:type git :flavor melpa :host github :repo "brotzeit/rustic" :package "rustic" :local-repo "rustic")) "rust-mode" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :host github :repo "rust-lang/rust-mode" :package "rust-mode" :local-repo "rust-mode")) "f" ("2022-01-28 13:35:42" ("s" "dash") (:type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "markdown-mode" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :host github :repo "jrblevin/markdown-mode" :package "markdown-mode" :local-repo "markdown-mode")) "project" ("2022-01-28 13:35:42" ("emacs" "xref") (:type git :host github :repo "emacs-straight/project" :files ("*" (:exclude ".git")) :package "project" :local-repo "project")) "xref" ("2022-01-28 13:35:42" ("emacs") (:type git :host github :repo "emacs-straight/xref" :files ("*" (:exclude ".git")) :package "xref" :local-repo "xref")) "spinner" ("2022-01-28 13:35:42" ("emacs") (:type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git")) :package "spinner" :local-repo "spinner")) "xterm-color" ("2022-01-28 13:35:42" ("emacs") (:type git :flavor melpa :host github :repo "atomontage/xterm-color" :package "xterm-color" :local-repo "xterm-color")) "lsp-mode" ("2022-01-25 07:28:16" ("emacs" "dash" "f" "ht" "spinner" "markdown-mode" "lv") (:type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode" :package "lsp-mode" :local-repo "lsp-mode")) "ht" ("2022-01-25 07:28:16" ("dash") (:type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "lv" ("2022-01-25 07:28:16" nil (:type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra" :package "lv" :local-repo "hydra")) "lsp-ui" ("2022-01-25 07:28:16" ("emacs" "dash" "lsp-mode" "markdown-mode") (:type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui" :package "lsp-ui" :local-repo "lsp-ui")) "all-the-icons" ("2021-12-11 11:00:11" ("emacs") (:type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el" :package "all-the-icons" :local-repo "all-the-icons.el")) "exwm" ("2022-01-28 13:35:42" ("xelb") (:type git :host github :repo "emacs-straight/exwm" :files ("*" (:exclude ".git")) :package "exwm" :local-repo "exwm")) "xelb" ("2022-01-28 13:35:42" ("emacs" "cl-generic") (:type git :host github :repo "emacs-straight/xelb" :files ("*" (:exclude ".git")) :package "xelb" :local-repo "xelb")) "selectrum" ("2022-01-28 13:35:41" ("emacs") (:host github :repo "raxod502/selectrum" :flavor melpa :package "selectrum" :type git :local-repo "selectrum")) "selectrum-prescient" ("2021-12-11 11:21:09" ("emacs" "prescient" "selectrum") (:type git :flavor melpa :files ("selectrum-prescient.el" "selectrum-prescient-pkg.el") :host github :repo "raxod502/prescient.el" :package "selectrum-prescient" :local-repo "prescient.el")) "prescient" ("2021-12-11 11:21:09" ("emacs") (:flavor melpa :files ("prescient.el" "prescient-pkg.el") :package "prescient" :local-repo "prescient.el" :type git :repo "raxod502/prescient.el" :host github)) "consult" ("2022-01-28 13:35:41" ("emacs") (:type git :flavor melpa :host github :repo "minad/consult" :package "consult" :local-repo "consult")) "marginalia" ("2022-01-28 13:35:41" ("emacs") (:type git :flavor melpa :host github :repo "minad/marginalia" :package "marginalia" :local-repo "marginalia")) "vertico" ("2022-01-28 13:35:41" ("emacs") (:type git :host github :repo "emacs-straight/vertico" :files ("*" (:exclude ".git")) :package "vertico" :local-repo "vertico")) "orderless" ("2022-01-28 13:35:41" ("emacs") (:type git :flavor melpa :host github :repo "oantolin/orderless" :package "orderless" :local-repo "orderless")) "corfu" ("2022-01-28 13:35:41" ("emacs") (:type git :host github :repo "emacs-straight/corfu" :files ("*" (:exclude ".git")) :package "corfu" :local-repo "corfu")) "ligature" ("2022-01-28 13:35:44" ("emacs") (:host github :repo "mickeynp/ligature.el" :package "ligature" :type git :local-repo "ligature.el")) "unicode-fonts" ("2022-01-28 13:35:44" ("font-utils" "ucs-utils" "list-utils" "persistent-soft" "pcache") (:type git :flavor melpa :host github :repo "rolandwalker/unicode-fonts" :package "unicode-fonts" :local-repo "unicode-fonts")) "font-utils" ("2022-01-28 13:35:44" ("persistent-soft" "pcache") (:type git :flavor melpa :host github :repo "rolandwalker/font-utils" :package "font-utils" :local-repo "font-utils")) "persistent-soft" ("2022-01-28 13:35:44" ("pcache" "list-utils") (:type git :flavor melpa :host github :repo "rolandwalker/persistent-soft" :package "persistent-soft" :local-repo "persistent-soft")) "pcache" ("2022-01-28 13:35:44" ("emacs") (:type git :flavor melpa :host github :repo "sigma/pcache" :package "pcache" :local-repo "pcache")) "list-utils" ("2022-01-28 13:35:44" nil (:type git :flavor melpa :host github :repo "rolandwalker/list-utils" :package "list-utils" :local-repo "list-utils")) "ucs-utils" ("2022-01-28 13:35:44" ("persistent-soft" "pcache" "list-utils") (:type git :flavor melpa :host github :repo "rolandwalker/ucs-utils" :package "ucs-utils" :local-repo "ucs-utils")) "almost-mono-themes" ("2022-01-28 13:35:46" ("emacs") (:type git :flavor melpa :host github :repo "cryon/almost-mono-themes" :package "almost-mono-themes" :local-repo "almost-mono-themes")) "hide-mode-line" ("2021-12-11 14:48:33" ("emacs") (:type git :flavor melpa :host github :repo "hlissner/emacs-hide-mode-line" :package "hide-mode-line" :local-repo "emacs-hide-mode-line")) "dashboard" ("2022-01-25 11:29:04" ("emacs") (:type git :flavor melpa :files (:defaults "banners" "dashboard-pkg.el") :host github :repo "emacs-dashboard/emacs-dashboard" :package "dashboard" :local-repo "emacs-dashboard")) "org-bullets" ("2021-12-12 09:05:02" nil (:type git :flavor melpa :host github :repo "integral-dw/org-bullets" :package "org-bullets" :local-repo "org-bullets")) "olivetti" ("2021-12-12 09:16:50" ("emacs") (:type git :flavor melpa :host github :repo "rnkn/olivetti" :package "olivetti" :local-repo "olivetti")) "org-roam" ("2022-01-28 13:35:46" ("emacs" "dash" "f" "org" "emacsql" "emacsql-sqlite" "magit-section") (:type git :flavor melpa :files (:defaults "extensions/*" "org-roam-pkg.el") :host github :repo "org-roam/org-roam" :package "org-roam" :local-repo "org-roam")) "emacsql" ("2022-01-28 13:35:46" ("emacs") (:type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql" :package "emacsql" :local-repo "emacsql")) "emacsql-sqlite" ("2022-01-28 13:35:46" ("emacs" "emacsql") (:flavor melpa :files ("emacsql-sqlite.el" "sqlite" "emacsql-sqlite-pkg.el") :package "emacsql-sqlite" :local-repo "emacsql" :type git :repo "skeeto/emacsql" :host github)) "deft" ("2022-01-28 13:35:46" nil (:type git :flavor melpa :host github :repo "jrblevin/deft" :package "deft" :local-repo "deft")) "mixed-pitch" ("2022-01-08 10:50:06" ("emacs") (:type git :flavor melpa :host gitlab :repo "jabranham/mixed-pitch" :package "mixed-pitch" :local-repo "mixed-pitch")) "emojify" ("2022-01-08 11:36:26" ("seq" "ht" "emacs") (:type git :flavor melpa :files (:defaults "data" "images" "emojify-pkg.el") :host github :repo "iqbalansari/emacs-emojify" :package "emojify" :local-repo "emacs-emojify")) "org-superstar" ("2022-01-28 13:35:41" ("org" "emacs") (:type git :flavor melpa :host github :repo "integral-dw/org-superstar-mode" :package "org-superstar" :local-repo "org-superstar-mode")) "org-habit-plus" ("2022-01-09 21:05:53" nil (:host github :repo "myshevchuk/org-habit-plus" :package "org-habit-plus" :type git :local-repo "org-habit-plus")) "org-dashboard" ("2022-01-28 13:35:46" ("cl-lib") (:type git :flavor melpa :host github :repo "bard/org-dashboard" :package "org-dashboard" :local-repo "org-dashboard")) "borland-blue-theme" ("2022-01-24 09:35:00" nil (:type git :flavor melpa :host github :repo "fourier/borland-blue-theme" :package "borland-blue-theme" :local-repo "borland-blue-theme")) "base16-theme" ("2022-01-24 10:19:07" nil (:type git :flavor melpa :files (:defaults "build/*.el" "base16-theme-pkg.el") :host github :repo "belak/base16-emacs" :package "base16-theme" :local-repo "base16-emacs")) "basic-theme" ("2022-01-24 17:10:08" ("emacs") (:type git :flavor melpa :host github :repo "fgeller/basic-theme.el" :package "basic-theme" :local-repo "basic-theme.el")) "color-theme-sanityinc-tomorrow" ("2022-01-24 17:54:24" nil (:type git :flavor melpa :host github :repo "purcell/color-theme-sanityinc-tomorrow" :package "color-theme-sanityinc-tomorrow" :local-repo "color-theme-sanityinc-tomorrow")) "rocket-chat" ("2022-01-28 13:35:48" ("cl-lib" "promise" "async-await" "request") (:host github :repo "4hiziri/rocket-chat" :package "rocket-chat" :type git :local-repo "rocket-chat")) "promise" ("2022-01-28 13:35:48" ("emacs") (:type git :flavor melpa :host github :repo "chuntaro/emacs-promise" :package "promise" :local-repo "emacs-promise")) "async-await" ("2022-01-28 13:35:48" ("emacs" "promise" "iter2") (:type git :flavor melpa :host github :repo "chuntaro/emacs-async-await" :package "async-await" :local-repo "emacs-async-await")) "iter2" ("2022-01-28 13:35:48" ("emacs") (:type git :flavor melpa :host github :repo "doublep/iter2" :package "iter2" :local-repo "iter2"))))

#s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("straight" ((straight-x straight straight-autoloads) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos directory.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t nil) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

(fn &optional SOURCES ACTION)" t nil) (autoload 'straight-visit-package-website "straight" "Interactively select a recipe, and visit the package's website." t nil) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil if package was actually installed, and nil
otherwise (this can only happen if NO-CLONE is non-nil).

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t nil) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)" nil nil) (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t nil) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t nil) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t nil) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t nil) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded." nil nil) (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted." nil nil) (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t nil) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t nil) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t nil) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t nil) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t nil) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t nil) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t nil) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package '(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function '0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t nil) (autoload 'straight-dependents "straight" "Return a list PACKAGE's dependents.

(fn &optional PACKAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight" '("straight-"))) (defvar straight-x-pinned-packages nil "List of pinned packages.") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "straight-x" '("straight-x-"))) (provide 'straight-autoloads)) "nano-emacs" ((nano-session nano-modeline nano-writer nano-compact nano-counsel nano-defaults nano-help nano-layout nano-theme nano nano-splash nano-colors nano-base-colors nano-command nano-emacs-autoloads nano-theme-dark nano-mu4e nano-agenda nano-minibuffer nano-bindings nano-faces nano-theme-light) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-agenda" '("center-string" "nano-agenda"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-base-colors" '("nano-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-bindings" '("nano--delete-frame-or-kill-emacs" "new-frame"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-colors" '("material-color" "nord-color" "open-color"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-command" '("nano-c"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-compact" '("nano-modeline-compose"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-defaults" '("copy-from-osx" "paste-to-osx"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-faces" '("nano-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-help" '("nano-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-modeline" '("buffer-menu-mode-header-line" "nano-mode" "shorten-directory" "vc-branch"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-mu4e" '("-mu4e-resize-headers-window" "mu4e-" "nano-mu4e-dashboard-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-session" '("unpropertize-kill-ring"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-splash" '("center-string" "mac-animation-" "nano-splash"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-theme" '("nano-theme" "set-face"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-theme-dark" '("nano-theme-set-dark"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-theme-light" '("nano-theme-set-light"))) (autoload 'writer-mode "nano-writer" "

(fn)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-writer" '("writer-mode--"))) (provide 'nano-emacs-autoloads)) "nano-theme" ((nano-light-theme nano-theme nano-theme-autoloads nano-dark-theme) (when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-dark-theme" '("nano-dark"))) (when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-light-theme" '("nano-light"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nano-theme" '("nano-"))) (provide 'nano-theme-autoloads)) "bind-key" ((bind-key-autoloads bind-key) (autoload 'bind-key "bind-key" "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

  (bind-key \"M-h\" #'some-interactive-function 'my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

(fn KEY-NAME COMMAND &optional KEYMAP PREDICATE)" nil t) (autoload 'unbind-key "bind-key" "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details.

(fn KEY-NAME &optional KEYMAP)" nil t) (autoload 'bind-key* "bind-key" "Similar to `bind-key', but overrides any mode-specific bindings.

(fn KEY-NAME COMMAND &optional PREDICATE)" nil t) (autoload 'bind-keys "bind-key" "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

(fn &rest ARGS)" nil t) (autoload 'bind-keys* "bind-key" "

(fn &rest ARGS)" nil t) (autoload 'describe-personal-keybindings "bind-key" "Display all the personal keybindings defined by `bind-key'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bind-key" '("bind-key" "compare-keybindings" "get-binding-description" "override-global-m" "personal-keybindings"))) (provide 'bind-key-autoloads)) "use-package" ((use-package use-package-delight use-package-bind-key use-package-diminish use-package-ensure use-package-lint use-package-autoloads use-package-core use-package-jump) (autoload 'use-package-autoload-keymap "use-package-bind-key" "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
deferred until the prefix key sequence is pressed.

(fn KEYMAP-SYMBOL PACKAGE OVERRIDE)" nil nil) (autoload 'use-package-normalize-binder "use-package-bind-key" "

(fn NAME KEYWORD ARGS)" nil nil) (defalias 'use-package-normalize/:bind 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind* 'use-package-normalize-binder) (defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode) (defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode) (autoload 'use-package-handler/:bind "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional BIND-MACRO)" nil nil) (defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder) (defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder) (autoload 'use-package-handler/:bind-keymap "use-package-bind-key" "

(fn NAME KEYWORD ARGS REST STATE &optional OVERRIDE)" nil nil) (autoload 'use-package-handler/:bind-keymap* "use-package-bind-key" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-bind-key" '("use-package-handler/:bind*"))) (autoload 'use-package "use-package-core" "Declare an Emacs package by specifying a group of configuration options.

For full documentation, please see the README file that came with
this file.  Usage:

  (use-package package-name
     [:keyword [option]]...)

:init            Code to run before PACKAGE-NAME has been loaded.
:config          Code to run after PACKAGE-NAME has been loaded.  Note that
                 if loading is deferred for any reason, this code does not
                 execute until the lazy load has occurred.
:preface         Code to be run before everything except `:disabled'; this
                 can be used to define functions for use in `:if', or that
                 should be seen by the byte-compiler.

:mode            Form to be added to `auto-mode-alist'.
:magic           Form to be added to `magic-mode-alist'.
:magic-fallback  Form to be added to `magic-fallback-mode-alist'.
:interpreter     Form to be added to `interpreter-mode-alist'.

:commands        Define autoloads for commands that will be defined by the
                 package.  This is useful if the package is being lazily
                 loaded, and you wish to conditionally call functions in your
                 `:init' block that are defined in the package.
:hook            Specify hook(s) to attach this package to.

:bind            Bind keys, and define autoloads for the bound commands.
:bind*           Bind keys, and define autoloads for the bound commands,
                 *overriding all minor mode bindings*.
:bind-keymap     Bind a key prefix to an auto-loaded keymap defined in the
                 package.  This is like `:bind', but for keymaps.
:bind-keymap*    Like `:bind-keymap', but overrides all minor mode bindings

:defer           Defer loading of a package -- this is implied when using
                 `:commands', `:bind', `:bind*', `:mode', `:magic', `:hook',
                 `:magic-fallback', or `:interpreter'.  This can be an integer,
                 to force loading after N seconds of idle time, if the package
                 has not already been loaded.
:after           Delay the use-package declaration until after the named modules
                 have loaded. Once load, it will be as though the use-package
                 declaration (without `:after') had been seen at that moment.
:demand          Prevent the automatic deferred loading introduced by constructs
                 such as `:bind' (see `:defer' for the complete list).

:if EXPR         Initialize and load only if EXPR evaluates to a non-nil value.
:disabled        The package is ignored completely if this keyword is present.
:defines         Declare certain variables to silence the byte-compiler.
:functions       Declare certain functions to silence the byte-compiler.
:load-path       Add to the `load-path' before attempting to load the package.
:diminish        Support for diminish.el (if installed).
:delight         Support for delight.el (if installed).
:custom          Call `custom-set' or `set-default' with each variable
                 definition without modifying the Emacs `custom-file'.
                 (compare with `custom-set-variables').
:custom-face     Call `customize-set-faces' with each face definition.
:ensure          Loads the package using package.el if necessary.
:pin             Pin the package to an archive.

(fn NAME &rest ARGS)" nil t) (function-put 'use-package 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-core" '("use-package-"))) (autoload 'use-package-normalize/:delight "use-package-delight" "Normalize arguments to delight.

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:delight "use-package-delight" "

(fn NAME KEYWORD ARGS REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-delight" '("use-package-normalize-delight"))) (autoload 'use-package-normalize/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:diminish "use-package-diminish" "

(fn NAME KEYWORD ARG REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-diminish" '("use-package-normalize-diminish"))) (autoload 'use-package-normalize/:ensure "use-package-ensure" "

(fn NAME KEYWORD ARGS)" nil nil) (autoload 'use-package-handler/:ensure "use-package-ensure" "

(fn NAME KEYWORD ENSURE REST STATE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-ensure" '("use-package-"))) (autoload 'use-package-jump-to-package-form "use-package-jump" "Attempt to find and jump to the `use-package' form that loaded
PACKAGE. This will only find the form if that form actually
required PACKAGE. If PACKAGE was previously required then this
function will jump to the file that originally required PACKAGE
instead.

(fn PACKAGE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-jump" '("use-package-find-require"))) (autoload 'use-package-lint "use-package-lint" "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "use-package-lint" '("use-package-lint-declaration"))) (provide 'use-package-autoloads)) "diminish" ((diminish-autoloads diminish) (autoload 'diminish "diminish" "Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish 'jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have abbrev-mode enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space.

(fn MODE &optional TO-WHAT)" t nil) (autoload 'diminish-undo "diminish" "Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked \\[diminish]).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).

(fn MODE)" t nil) (autoload 'diminished-modes "diminish" "Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "diminish" '("diminish"))) (provide 'diminish-autoloads)) "ivy" ((ivy elpa ivy-faces ivy-overlay ivy-autoloads colir) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "colir" '("colir-"))) (autoload 'ivy-resume "ivy" "Resume the last completion session, or SESSION if non-nil.
With a prefix arg, try to restore a recorded completion session,
if one exists.

(fn &optional SESSION)" t nil) (autoload 'ivy-read "ivy" "Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table, supplied for `minibuffer-completion-table'.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

PRESELECT, when non-nil, determines which one of the candidates
matching INITIAL-INPUT to select initially.  An integer stands
for the position of the desired candidate in the collection,
counting from zero.  Otherwise, use the first occurrence of
PRESELECT in the collection.  Comparison is first done with
`equal'.  If that fails, and when applicable, match PRESELECT as
a regular expression.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is re-displayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes one argument, the selected candidate. If COLLECTION is
an alist, the argument is a cons cell, otherwise it's a string.

MULTI-ACTION, when non-nil, is called instead of ACTION when
there are marked candidates. It takes the list of candidates as
its only argument. When it's nil, ACTION is called on each marked
candidate.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

EXTRA-PROPS is a plist that can be used to store
collection-specific session-specific data.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session.

(fn PROMPT COLLECTION &key PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY PRESELECT DEF KEYMAP UPDATE-FN SORT ACTION MULTI-ACTION UNWIND RE-BUILDER MATCHER DYNAMIC-COLLECTION EXTRA-PROPS CALLER)" nil nil) (autoload 'ivy-completing-read "ivy" "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value or a symbol.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored.

(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HISTORY DEF INHERIT-INPUT-METHOD)" nil nil) (defvar ivy-mode nil "Non-nil if Ivy mode is enabled.
See the `ivy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-mode'.") (custom-autoload 'ivy-mode "ivy" nil) (autoload 'ivy-mode "ivy" "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}

(fn &optional ARG)" t nil) (autoload 'ivy-switch-buffer "ivy" "Switch to another buffer." t nil) (autoload 'ivy-switch-view "ivy" "Switch to one of the window views stored by `ivy-push-view'." t nil) (autoload 'ivy-switch-buffer-other-window "ivy" "Switch to another buffer in another window." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy" '("ivy-" "with-ivy-window"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-overlay" '("ivy-"))) (provide 'ivy-autoloads)) "swiper" ((swiper-autoloads swiper) (autoload 'swiper-avy "swiper" "Jump to one of the current swiper candidates with `avy'." t nil) (autoload 'swiper-backward "swiper" "`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-thing-at-point "swiper" "`swiper' with `ivy-thing-at-point'." t nil) (autoload 'swiper-all-thing-at-point "swiper" "`swiper-all' with `ivy-thing-at-point'." t nil) (autoload 'swiper "swiper" "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-all "swiper" "Run `swiper' for all open buffers.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-isearch "swiper" "A `swiper' that's not line-based.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'swiper-isearch-backward "swiper" "Like `swiper-isearch' but the first result is before the point.

(fn &optional INITIAL-INPUT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "swiper" '("swiper-"))) (provide 'swiper-autoloads)) "counsel" ((counsel counsel-autoloads) (autoload 'counsel-company "counsel" "Complete using `company-candidates'." t nil) (autoload 'counsel-irony "counsel" "Inline C/C++ completion using Irony." t nil) (autoload 'counsel-describe-variable "counsel" "Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'." t nil) (autoload 'counsel-describe-function "counsel" "Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'." t nil) (autoload 'counsel-describe-symbol "counsel" "Forward to `describe-symbol'." t nil) (autoload 'counsel-set-variable "counsel" "Set a variable SYM, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

With a prefix arg, restrict list to variables defined using
`defcustom'.

(fn SYM)" t nil) (autoload 'counsel-apropos "counsel" "Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them." t nil) (autoload 'counsel-info-lookup-symbol "counsel" "Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered.

(fn SYMBOL &optional MODE)" t nil) (autoload 'counsel-M-x "counsel" "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-command-history "counsel" "Show the history of commands." t nil) (autoload 'counsel-load-library "counsel" "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil) (autoload 'counsel-find-library "counsel" "Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'." t nil) (autoload 'counsel-load-theme "counsel" "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'." t nil) (autoload 'counsel-descbinds "counsel" "Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one.

(fn &optional PREFIX BUFFER)" t nil) (autoload 'counsel-describe-face "counsel" "Completion for `describe-face'." t nil) (autoload 'counsel-faces "counsel" "Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face." t nil) (autoload 'counsel-git "counsel" "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-git-grep "counsel" "Grep for a string in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY CMD)" t nil) (autoload 'counsel-git-stash "counsel" "Search through all available git stashes." t nil) (autoload 'counsel-git-change-worktree "counsel" "Find the file corresponding to the current buffer on a different worktree." t nil) (autoload 'counsel-git-checkout "counsel" "Call the \"git checkout\" command." t nil) (autoload 'counsel-git-log "counsel" "Call the \"git log --grep\" shell command." t nil) (autoload 'counsel-find-file "counsel" "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-dired "counsel" "Forward to `dired'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-recentf "counsel" "Find a file on `recentf-list'." t nil) (autoload 'counsel-buffer-or-recentf "counsel" "Find a buffer visiting a file or file on `recentf-list'." t nil) (autoload 'counsel-bookmark "counsel" "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist." t nil) (autoload 'counsel-bookmarked-directory "counsel" "Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'." t nil) (autoload 'counsel-file-register "counsel" "Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file." t nil) (autoload 'counsel-locate-action-extern "counsel" "Pass X to `xdg-open' or equivalent command via the shell.

(fn X)" t nil) (autoload 'counsel-locate "counsel" "Call a \"locate\" style shell command.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-tracker "counsel" nil t nil) (autoload 'counsel-fzf "counsel" "Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY FZF-PROMPT)" t nil) (autoload 'counsel-dpkg "counsel" "Call the \"dpkg\" shell command." t nil) (autoload 'counsel-rpm "counsel" "Call the \"rpm\" shell command." t nil) (autoload 'counsel-file-jump "counsel" "Jump to a file below the current directory.
List all files within the current directory or any of its sub-directories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-dired-jump "counsel" "Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY)" t nil) (autoload 'counsel-ag "counsel" "Grep for a string in a root directory using `ag'.

By default, the root directory is the first directory containing
a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, prompt additionally for EXTRA-AG-ARGS.

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-AG-ARGS AG-PROMPT &key CALLER)" t nil) (autoload 'counsel-pt "counsel" "Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-ack "counsel" "Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-rg "counsel" "Grep for a string in the current directory using `rg'.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el

(fn &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)" t nil) (autoload 'counsel-grep "counsel" "Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-backward "counsel" "Grep for a string in the file visited by the current buffer going
backward similar to `swiper-backward'. When non-nil, INITIAL-INPUT is
the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-or-swiper "counsel" "Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-grep-or-swiper-backward "counsel" "Call `swiper-backward' for small buffers and `counsel-grep-backward' for
large ones.  When non-nil, INITIAL-INPUT is the initial search pattern.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel-recoll "counsel" "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input.

(fn &optional INITIAL-INPUT)" t nil) (autoload 'counsel--org-get-tags "counsel" nil nil nil) (autoload 'counsel-org-tag "counsel" "Add or remove tags in `org-mode'." t nil) (autoload 'counsel-org-tag-agenda "counsel" "Set tags for the current agenda item." t nil) (defalias 'counsel-org-goto #'counsel-outline) (autoload 'counsel-org-goto-all "counsel" "Go to a different location in any org file." t nil) (autoload 'counsel-org-file "counsel" "Browse all attachments for current Org file." t nil) (autoload 'counsel-org-entity "counsel" "Complete Org entities using Ivy." t nil) (autoload 'counsel-org-capture "counsel" "Capture something." t nil) (autoload 'counsel-org-agenda-headlines "counsel" "Choose from headers of `org-mode' files in the agenda." t nil) (autoload 'counsel-org-link "counsel" "Insert a link to an headline with completion." t nil) (autoload 'counsel-mark-ring "counsel" "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see." t nil) (autoload 'counsel-evil-marks "counsel" "Ivy replacement for `evil-show-marks'.
By default, this function respects `counsel-evil-marks-exclude-registers'.
When ARG is non-nil, display all active evil registers.

(fn &optional ARG)" t nil) (autoload 'counsel-package "counsel" "Install or delete packages.

Packages not currently installed are prefixed with \"+\", and
selecting one of these will try to install it.
Packages currently installed are prefixed with \"-\", and
selecting one of these will try to delete it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Describe package
  \\[ivy-dispatching-done] h: Visit package's homepage" t nil) (autoload 'counsel-tmm "counsel" "Text-mode emulation of looking and choosing from a menu bar." t nil) (autoload 'counsel-yank-pop "counsel" "Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted.

(fn &optional ARG)" t nil) (autoload 'counsel-register "counsel" "Interactively choose a register." t nil) (autoload 'counsel-evil-registers "counsel" "Ivy replacement for `evil-show-registers'." t nil) (autoload 'counsel-imenu "counsel" "Jump to a buffer position indexed by imenu." t nil) (autoload 'counsel-list-processes "counsel" "Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer." t nil) (autoload 'counsel-minibuffer-history "counsel" "Browse minibuffer history." t nil) (autoload 'counsel-esh-history "counsel" "Browse Eshell history." t nil) (autoload 'counsel-shell-history "counsel" "Browse shell history." t nil) (autoload 'counsel-slime-repl-history "counsel" "Browse Slime REPL history." t nil) (autoload 'counsel-hydra-heads "counsel" "Call a head of the current/last hydra." t nil) (autoload 'counsel-semantic "counsel" "Jump to a semantic tag in the current buffer." t nil) (autoload 'counsel-semantic-or-imenu "counsel" nil t nil) (autoload 'counsel-outline "counsel" "Jump to an outline heading with completion." t nil) (autoload 'counsel-ibuffer "counsel" "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\").

(fn &optional NAME)" t nil) (autoload 'counsel-switch-to-shell-buffer "counsel" "Switch to a shell buffer, or create one." t nil) (autoload 'counsel-unicode-char "counsel" "Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1.

(fn &optional COUNT)" t nil) (autoload 'counsel-colors-emacs "counsel" "Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil) (autoload 'counsel-colors-web "counsel" "Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color." t nil) (autoload 'counsel-fonts "counsel" "Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font." t nil) (autoload 'counsel-kmacro "counsel" "Interactively choose and run a keyboard macro.

With prefix argument, run macro that many times.

Macros are run using the current value of `kmacro-counter-value'
and their respective counter format. Displayed next to each macro is
the counter's format and initial value.

One can use actions to copy the counter format or initial counter
value of a macro, using them for a new macro." t nil) (autoload 'counsel-geiser-doc-look-up-manual "counsel" "Search Scheme documentation." t nil) (autoload 'counsel-rhythmbox "counsel" "Choose a song from the Rhythmbox library to play or enqueue.

(fn &optional ARG)" t nil) (autoload 'counsel-linux-app "counsel" "Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files.

(fn &optional ARG)" t nil) (autoload 'counsel-wmctrl "counsel" "Select a desktop window using wmctrl." t nil) (autoload 'counsel-switch-buffer "counsel" "Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil) (autoload 'counsel-switch-buffer-other-window "counsel" "Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window." t nil) (autoload 'counsel-compile "counsel" "Call `compile' completing with smart suggestions, optionally for DIR.

Additional actions:

\\{counsel-compile-map}

(fn &optional DIR)" t nil) (autoload 'counsel-compile-env "counsel" "Update `counsel-compile-env' interactively." t nil) (autoload 'counsel-minor "counsel" "Enable or disable minor mode.

Disabled minor modes are prefixed with \"+\", and
selecting one of these will enable it.
Enabled minor modes are prefixed with \"-\", and
selecting one of these will enable it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Go to minor mode definition
  \\[ivy-dispatching-done] h: Describe minor mode" t nil) (autoload 'counsel-major "counsel" nil t nil) (autoload 'counsel-compilation-errors "counsel" "Compilation errors." t nil) (autoload 'counsel-flycheck "counsel" "Flycheck errors." t nil) (defvar counsel-mode nil "Non-nil if Counsel mode is enabled.
See the `counsel-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `counsel-mode'.") (custom-autoload 'counsel-mode "counsel" nil) (autoload 'counsel-mode "counsel" "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "counsel" '("counsel-" "ivy-function-called-at-point" "tmm-km-list"))) (provide 'counsel-autoloads)) "org" ((ob-ditaa ob-C ob-css ob-tangle org-indent ol-gnus ob-groovy org-keys oc-biblatex ol-bbdb ob-table org-install org-table ol-rmail ob-ruby ol-mhe ob-js oc-bibtex org-macs org-habit ob-gnuplot ob-haskell ob-lilypond ox-latex ox org-loaddefs ob-latex ox-texinfo org-duration ob-sqlite org-crypt ob-awk ob-java ob-processing ob-ocaml oc ob-screen ob-core ol-man ob-perl org-colview org-src org-inlinetask ob-eval ob-lob ox-html ob-python ob-lisp ob-ref org-num ob-matlab ob-sed org-compat ob-sql ox-publish ob ob-org org-mobile ox-man org-goto ob-comint org ob-R org-plot ob-octave org-lint ox-ascii org-pcomplete ob-dot ol-docview ob-fortran ob-scheme ob-julia org-archive ob-sass org-list org-protocol ol-doi org-footnote ob-shell ox-md org-datetree ox-icalendar ob-forth ox-beamer ol-irc org-id ol-info ob-maxima ob-lua org-version org-capture ol-eshell org-clock ol-w3m ob-emacs-lisp ob-exp ol org-macro org-element ox-org org-agenda ox-odt ob-calc oc-csl oc-basic org-tempo ob-clojure ob-eshell org-attach-git org-persist org-faces org-refile ol-bibtex org-timer org-mouse ox-koma-letter org-attach ol-eww ob-makefile oc-natbib org-ctags ob-plantuml org-feed org-entities)) "request" ((request request-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "request" '("request-"))) (provide 'request-autoloads)) "deferred" ((deferred-autoloads deferred) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deferred" '("deferred:"))) (provide 'deferred-autoloads)) "request-deferred" ((request-deferred-autoloads request-deferred) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "request-deferred" '("request-deferred"))) (provide 'request-deferred-autoloads)) "gntp" ((gntp-autoloads gntp) (autoload 'gntp-notify "gntp" "Send notification NAME with TITLE, TEXT, PRIORITY and ICON to SERVER:PORT.
PORT defaults to `gntp-server-port'

(fn NAME TITLE TEXT SERVER &optional PORT PRIORITY ICON)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gntp" '("gntp-"))) (provide 'gntp-autoloads)) "log4e" ((log4e-autoloads log4e) (autoload 'log4e-mode "log4e" "Major mode for browsing a buffer made by log4e.

\\<log4e-mode-map>
\\{log4e-mode-map}

(fn)" t nil) (autoload 'log4e:insert-start-log-quickly "log4e" "Insert logging statment for trace level log at start of current function/macro." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "log4e" '("log4e"))) (provide 'log4e-autoloads)) "alert" ((alert-autoloads alert) (autoload 'alert-add-rule "alert" "Programmatically add an alert configuration rule.

Normally, users should custoimze `alert-user-configuration'.
This facility is for module writers and users that need to do
things the Lisp way.

Here is a rule the author currently uses with ERC, so that the
fringe gets colored whenever people chat on BitlBee:

(alert-add-rule :status   \\='(buried visible idle)
                :severity \\='(moderate high urgent)
                :mode     \\='erc-mode
                :predicate
                #\\='(lambda (info)
                    (string-match (concat \"\\\\`[^&].*@BitlBee\\\\\\='\")
                                  (erc-format-target-and/or-network)))
                :persistent
                #\\='(lambda (info)
                    ;; If the buffer is buried, or the user has been
                    ;; idle for `alert-reveal-idle-time' seconds,
                    ;; make this alert persistent.  Normally, alerts
                    ;; become persistent after
                    ;; `alert-persist-idle-time' seconds.
                    (memq (plist-get info :status) \\='(buried idle)))
                :style \\='fringe
                :continue t)

(fn &key SEVERITY STATUS MODE CATEGORY TITLE MESSAGE PREDICATE ICON (STYLE alert-default-style) PERSISTENT CONTINUE NEVER-PERSIST APPEND)" nil nil) (autoload 'alert "alert" "Alert the user that something has happened.
MESSAGE is what the user will see.  You may also use keyword
arguments to specify additional details.  Here is a full example:

(alert \"This is a message\"
       :severity \\='high          ;; The default severity is `normal'
       :title \"Title\"           ;; An optional title
       :category \\='example       ;; A symbol to identify the message
       :mode \\='text-mode         ;; Normally determined automatically
       :buffer (current-buffer) ;; This is the default
       :data nil                ;; Unused by alert.el itself
       :persistent nil          ;; Force the alert to be persistent;
                                ;; it is best not to use this
       :never-persist nil       ;; Force this alert to never persist
       :id \\='my-id)              ;; Used to replace previous message of
                                ;; the same id in styles that support it
       :style \\='fringe)          ;; Force a given style to be used;
                                ;; this is only for debugging!

If no :title is given, the buffer-name of :buffer is used.  If
:buffer is nil, it is the current buffer at the point of call.

:data is an opaque value which modules can pass through to their
own styles if they wish.

Here are some more typical examples of usage:

  ;; This is the most basic form usage
  (alert \"This is an alert\")

  ;; You can adjust the severity for more important messages
  (alert \"This is an alert\" :severity \\='high)

  ;; Or decrease it for purely informative ones
  (alert \"This is an alert\" :severity \\='trivial)

  ;; Alerts can have optional titles.  Otherwise, the title is the
  ;; buffer-name of the (current-buffer) where the alert originated.
  (alert \"This is an alert\" :title \"My Alert\")

  ;; Further, alerts can have categories.  This allows users to
  ;; selectively filter on them.
  (alert \"This is an alert\" :title \"My Alert\"
         :category \\='some-category-or-other)

(fn MESSAGE &key (SEVERITY \\='normal) TITLE ICON CATEGORY BUFFER MODE DATA STYLE PERSISTENT NEVER-PERSIST ID)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "alert" '("alert-" "x-urgen"))) (provide 'alert-autoloads)) "persist" ((persist persist-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "persist" '("persist-"))) (provide 'persist-autoloads)) "org-gcal" ((org-gcal org-gcal-autoloads org-generic-id) (autoload 'org-gcal-sync "org-gcal" "Import events from calendars.
Export the ones to the calendar if unless
SKIP-EXPORT.  Set SILENT to non-nil to inhibit notifications.

(fn &optional SKIP-EXPORT SILENT)" t nil) (autoload 'org-gcal-fetch "org-gcal" "Fetch event data from google calendar." t nil) (autoload 'org-gcal-sync-buffer "org-gcal" "Sync entries with Calendar events in currently-visible portion of buffer.

Updates events on the server unless SKIP-EXPORT is set. In this case, events
modified on the server will overwrite entries in the buffer.
Set SILENT to non-nil to inhibit notifications.
Set FILTER-DATE to only update events scheduled for later than
\342\200\230org-gcal-up-days' and earlier than \342\200\230org-gcal-down-days'.
Set FILTER-MAANGED to only update events with \342\200\230org-gcal-managed-property\342\200\231 set
to \342\200\234org\342\200\235.

(fn &optional SKIP-EXPORT SILENT FILTER-DATE FILTER-MANAGED)" t nil) (autoload 'org-gcal-fetch-buffer "org-gcal" "Fetch changes to events in the currently-visible portion of the buffer

Unlike \342\200\230org-gcal-sync-buffer\342\200\231, this will not push any changes to Google
Calendar. For SILENT and FILTER-DATE see \342\200\230org-gcal-sync-buffer\342\200\231.

(fn &optional SILENT FILTER-DATE)" t nil) (autoload 'org-gcal-toggle-debug "org-gcal" "Toggle debugging flags for \342\200\230org-gcal'." t nil) (autoload 'org-gcal-post-at-point "org-gcal" "Post entry at point to current calendar.

This overwrites the event on the server with the data from the entry, except if
the \342\200\230org-gcal-etag-property\342\200\231 is present and is out of sync with the server, in
which case the entry is overwritten with data from the server instead.

If SKIP-IMPORT is not nil, don\342\200\231t overwrite the entry with data from the server.
If SKIP-EXPORT is not nil, don\342\200\231t overwrite the event on the server.
For valid values of EXISTING-MODE see
\342\200\230org-gcal-managed-post-at-point-update-existing'.

(fn &optional SKIP-IMPORT SKIP-EXPORT EXISTING-MODE)" t nil) (autoload 'org-gcal-delete-at-point "org-gcal" "Delete entry at point to current calendar." t nil) (autoload 'org-gcal-sync-tokens-clear "org-gcal" "Clear all Calendar API sync tokens.

  Use this to force retrieving all events in \342\200\230org-gcal-sync\342\200\231 or
  \342\200\230org-gcal-fetch\342\200\231." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gcal" '("org-gcal-"))) (autoload 'org-generic-id-get "org-generic-id" "Get the ID-PROP property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
In any case, the ID of the entry is returned.

(fn &optional ID-PROP POM)" nil nil) (autoload 'org-generic-id-find "org-generic-id" "Return the location of the entry with property ID-PROP, value ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker.

Normally, if an entry with ID is not found, this function will run
\342\200\230org-generic-id-update-id-locations' in order to pick up any updates to the
files, and then search again, before concluding an ID can\342\200\231t be found. If
CACHED is passed, that function will not be run.

Normally the ID will be searched for in the current buffer before updating ID
locations. This behavior can be disabled with NO-FALLBACK.

(fn ID-PROP ID &optional MARKERP CACHED NO-FALLBACK)" nil nil) (autoload 'org-generic-id-update-id-locations "org-generic-id" "Scan relevant files for IDs.
Store the relation between files and corresponding IDs.
This will scan all agenda files, all associated archives, and all
files currently mentioned in `org-generic-id-locations'.
When FILES is given, scan also these files.

(fn ID-PROP &optional FILES SILENT)" t nil) (autoload 'org-generic-id-locations-load "org-generic-id" "Read the data from `org-generic-id-locations-file'." nil nil) (autoload 'org-generic-id-add-location "org-generic-id" "Add the ID with location FILE to the database of ID locations.

(fn ID-PROP ID FILE)" nil nil) (autoload 'org-generic-id-find-id-file "org-generic-id" "Query the id database for the file in which this ID is located.

If NO-FALLBACK is set, don\342\200\231t fall back to current buffer if not found in
\342\200\230org-generic-id-locations\342\200\231.

(fn ID-PROP ID &optional NO-FALLBACK)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-generic-id" '("org-generic-id-"))) (provide 'org-gcal-autoloads)) "undo-tree" ((undo-tree-autoloads undo-tree) (autoload 'undo-tree-mode "undo-tree" "Toggle undo-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

If called interactively, enable Undo-Tree mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

(fn &optional ARG)" t nil) (put 'global-undo-tree-mode 'globalized-minor-mode t) (defvar global-undo-tree-mode nil "Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.") (custom-autoload 'global-undo-tree-mode "undo-tree" nil) (autoload 'global-undo-tree-mode "undo-tree" "Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.
See `undo-tree-mode' for more information on Undo-Tree mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-"))) (provide 'undo-tree-autoloads)) "dash" ((dash dash-autoloads) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

If called interactively, enable Dash-Fontify mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

(fn &optional ARG)" t nil) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.
See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t nil) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-"))) (provide 'dash-autoloads)) "popwin" ((popwin-autoloads popwin) (autoload 'popwin:popup-buffer "popwin" "Show BUFFER in a popup window and return the popup window. If
NOSELECT is non-nil, the popup window will not be selected. If
STICK is non-nil, the popup window will be stuck. If TAIL is
non-nil, the popup window will show the last contents. Calling
`popwin:popup-buffer' during `popwin:popup-buffer' is allowed. In
that case, the buffer of the popup window will be replaced with
BUFFER.

(fn BUFFER &key (WIDTH popwin:popup-window-width) (HEIGHT popwin:popup-window-height) (POSITION popwin:popup-window-position) NOSELECT DEDICATED STICK TAIL)" t nil) (autoload 'popwin:display-buffer "popwin" "Display BUFFER-OR-NAME, if possible, in a popup window, or as usual.
This function can be used as a value of
`display-buffer-function'.

(fn BUFFER-OR-NAME &optional NOT-THIS-WINDOW)" t nil) (autoload 'popwin:pop-to-buffer "popwin" "Same as `pop-to-buffer' except that this function will use `popwin:display-buffer-1' instead of `display-buffer'.  BUFFER,
OTHER-WINDOW amd NORECORD are the same arguments.

(fn BUFFER &optional OTHER-WINDOW NORECORD)" t nil) (autoload 'popwin:universal-display "popwin" "Call the following command interactively with letting `popwin:special-display-config' be `popwin:universal-display-config'.
This will be useful when displaying buffers in popup windows temporarily." t nil) (autoload 'popwin:one-window "popwin" "Delete other window than the popup window. C-g restores the original window configuration." t nil) (autoload 'popwin:popup-buffer-tail "popwin" "Same as `popwin:popup-buffer' except that the buffer will be `recenter'ed at the bottom.

(fn &rest SAME-AS-POPWIN:POPUP-BUFFER)" t nil) (autoload 'popwin:find-file "popwin" "Edit file FILENAME with popup window by `popwin:popup-buffer'.

(fn FILENAME &optional WILDCARDS)" t nil) (autoload 'popwin:find-file-tail "popwin" "Edit file FILENAME with popup window by `popwin:popup-buffer-tail'.

(fn FILE &optional WILDCARD)" t nil) (autoload 'popwin:messages "popwin" "Display *Messages* buffer in a popup window." t nil) (defvar popwin-mode nil "Non-nil if Popwin mode is enabled.
See the `popwin-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `popwin-mode'.") (custom-autoload 'popwin-mode "popwin" nil) (autoload 'popwin-mode "popwin" "Minor mode for `popwin-mode'.

If called interactively, enable Popwin mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "popwin" '("popwin:"))) (provide 'popwin-autoloads)) "s" ((s s-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "s" '("s-"))) (provide 's-autoloads)) "guide-key" ((guide-key-autoloads guide-key) (defvar guide-key-mode nil "Non-nil if Guide-Key mode is enabled.
See the `guide-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `guide-key-mode'.") (custom-autoload 'guide-key-mode "guide-key" nil) (autoload 'guide-key-mode "guide-key" "Toggle guide key mode.

In guide key mode, Guide following keys to an input key sequence
automatically and dynamically.
With a prefix argument ARG, enable guide key mode if ARG is
positive, otherwise disable.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "guide-key" '("guide-key/"))) (provide 'guide-key-autoloads)) "projectile" ((projectile-autoloads projectile) (autoload 'projectile-version "projectile" "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

(fn &optional SHOW-VERSION)" t nil) (autoload 'projectile-invalidate-cache "projectile" "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument PROMPT prompts for the name of the project whose cache
to invalidate.

(fn PROMPT)" t nil) (autoload 'projectile-purge-file-from-cache "projectile" "Purge FILE from the cache of the current project.

(fn FILE)" t nil) (autoload 'projectile-purge-dir-from-cache "projectile" "Purge DIR from the cache of the current project.

(fn DIR)" t nil) (autoload 'projectile-cache-current-file "projectile" "Add the currently visited file to the cache." t nil) (autoload 'projectile-discover-projects-in-directory "projectile" "Discover any projects in DIRECTORY and add them to the projectile cache.

If DEPTH is non-nil recursively descend exactly DEPTH levels below DIRECTORY and
discover projects there.

(fn DIRECTORY &optional DEPTH)" t nil) (autoload 'projectile-discover-projects-in-search-path "projectile" "Discover projects in `projectile-project-search-path'.
Invoked automatically when `projectile-mode' is enabled." t nil) (autoload 'projectile-switch-to-buffer "projectile" "Switch to a project buffer." t nil) (autoload 'projectile-switch-to-buffer-other-window "projectile" "Switch to a project buffer and show it in another window." t nil) (autoload 'projectile-switch-to-buffer-other-frame "projectile" "Switch to a project buffer and show it in another frame." t nil) (autoload 'projectile-display-buffer "projectile" "Display a project buffer in another window without selecting it." t nil) (autoload 'projectile-project-buffers-other-buffer "projectile" "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned." t nil) (autoload 'projectile-multi-occur "projectile" "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context.

(fn &optional NLINES)" t nil) (autoload 'projectile-find-other-file "projectile" "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-window "projectile" "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-other-file-other-frame "projectile" "Switch between files with the same name but different extensions in other frame.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

(fn &optional FLEX-MATCHING)" t nil) (autoload 'projectile-find-file-dwim "projectile" "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-window "projectile" "Jump to a project's files using completion based on context in other window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-dwim-other-frame "projectile" "Jump to a project's files using completion based on context in other frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file "projectile" "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-window "projectile" "Jump to a project's file using completion and show it in another window.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-file-other-frame "projectile" "Jump to a project's file using completion and show it in another frame.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-toggle-project-read-only "projectile" "Toggle project read only." t nil) (autoload 'projectile-find-dir "projectile" "Jump to a project's directory using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-window "projectile" "Jump to a project's directory in other window using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-dir-other-frame "projectile" "Jump to a project's directory in other frame using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-test-file "projectile" "Jump to a project's test file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

(fn &optional INVALIDATE-CACHE)" t nil) (autoload 'projectile-find-related-file-other-window "projectile" "Open related file in other window." t nil) (autoload 'projectile-find-related-file-other-frame "projectile" "Open related file in other frame." t nil) (autoload 'projectile-find-related-file "projectile" "Open related file." t nil) (autoload 'projectile-related-files-fn-groups "projectile" "Generate a related-files-fn which relates as KIND for files in each of GROUPS.

(fn KIND GROUPS)" nil nil) (autoload 'projectile-related-files-fn-extensions "projectile" "Generate a related-files-fn which relates as KIND for files having EXTENSIONS.

(fn KIND EXTENSIONS)" nil nil) (autoload 'projectile-related-files-fn-test-with-prefix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-PREFIX.

(fn EXTENSION TEST-PREFIX)" nil nil) (autoload 'projectile-related-files-fn-test-with-suffix "projectile" "Generate a related-files-fn which relates tests and impl for files with EXTENSION based on TEST-SUFFIX.

(fn EXTENSION TEST-SUFFIX)" nil nil) (autoload 'projectile-project-info "projectile" "Display info for current project." t nil) (autoload 'projectile-find-implementation-or-test-other-window "projectile" "Open matching implementation or test file in other window." t nil) (autoload 'projectile-find-implementation-or-test-other-frame "projectile" "Open matching implementation or test file in other frame." t nil) (autoload 'projectile-toggle-between-implementation-and-test "projectile" "Toggle between an implementation file and its test file." t nil) (autoload 'projectile-grep "projectile" "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp.

(fn &optional REGEXP ARG)" t nil) (autoload 'projectile-ag "projectile" "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-ripgrep "projectile" "Run a ripgrep (rg) search with `SEARCH-TERM' at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages ripgrep or rg being
installed to work.

(fn SEARCH-TERM &optional ARG)" t nil) (autoload 'projectile-regenerate-tags "projectile" "Regenerate the project's [e|g]tags." t nil) (autoload 'projectile-find-tag "projectile" "Find tag in project." t nil) (autoload 'projectile-run-command-in-root "projectile" "Invoke `execute-extended-command' in the project's root." t nil) (autoload 'projectile-run-shell-command-in-root "projectile" "Invoke `shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t nil) (autoload 'projectile-run-async-shell-command-in-root "projectile" "Invoke `async-shell-command' in the project's root.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER)" t nil) (autoload 'projectile-run-gdb "projectile" "Invoke `gdb' in the project's root." t nil) (autoload 'projectile-run-shell "projectile" "Invoke `shell' in the project's root.

Switch to the project specific shell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-eshell "projectile" "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-ielm "projectile" "Invoke `ielm' in the project's root.

Switch to the project specific ielm buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-term "projectile" "Invoke `term' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-run-vterm "projectile" "Invoke `vterm' in the project's root.

Switch to the project specific term buffer if it already exists.

Use a prefix argument ARG to indicate creation of a new process instead.

(fn &optional ARG)" t nil) (autoload 'projectile-replace "projectile" "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-replace-regexp "projectile" "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement.

(fn &optional ARG)" t nil) (autoload 'projectile-kill-buffers "projectile" "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'." t nil) (autoload 'projectile-save-project-buffers "projectile" "Save all project buffers." t nil) (autoload 'projectile-dired "projectile" "Open `dired' at the root of the project." t nil) (autoload 'projectile-dired-other-window "projectile" "Open `dired'  at the root of the project in another window." t nil) (autoload 'projectile-dired-other-frame "projectile" "Open `dired' at the root of the project in another frame." t nil) (autoload 'projectile-vc "projectile" "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available.

If PROJECT-ROOT is given, it is opened instead of the project
root directory of the current buffer file.  If interactively
called with a prefix argument, the user is prompted for a project
directory to open.

(fn &optional PROJECT-ROOT)" t nil) (autoload 'projectile-recentf "projectile" "Show a list of recently visited files in a project." t nil) (autoload 'projectile-configure-project "projectile" "Run project configure command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-compile-project "projectile" "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-test-project "projectile" "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-install-project "projectile" "Run project install command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-package-project "projectile" "Run project package command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-run-project "projectile" "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG.

(fn ARG)" t nil) (autoload 'projectile-repeat-last-command "projectile" "Run last projectile external command.

External commands are: `projectile-configure-project',
`projectile-compile-project', `projectile-test-project',
`projectile-install-project', `projectile-package-project',
and `projectile-run-project'.

If the prefix argument SHOW_PROMPT is non nil, the command can be edited.

(fn SHOW-PROMPT)" t nil) (autoload 'projectile-switch-project "projectile" "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-switch-open-project "projectile" "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'

(fn &optional ARG)" t nil) (autoload 'projectile-find-file-in-directory "projectile" "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in.

(fn &optional DIRECTORY)" t nil) (autoload 'projectile-find-file-in-known-projects "projectile" "Jump to a file in any of the known projects." t nil) (autoload 'projectile-cleanup-known-projects "projectile" "Remove known projects that don't exist anymore." t nil) (autoload 'projectile-clear-known-projects "projectile" "Clear both `projectile-known-projects' and `projectile-known-projects-file'." t nil) (autoload 'projectile-reset-known-projects "projectile" "Clear known projects and rediscover." t nil) (autoload 'projectile-remove-known-project "projectile" "Remove PROJECT from the list of known projects.

(fn &optional PROJECT)" t nil) (autoload 'projectile-remove-current-project-from-known-projects "projectile" "Remove the current project from the list of known projects." t nil) (autoload 'projectile-add-known-project "projectile" "Add PROJECT-ROOT to the list of known projects.

(fn PROJECT-ROOT)" t nil) (autoload 'projectile-ibuffer "projectile" "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied.

(fn PROMPT-FOR-PROJECT)" t nil) (autoload 'projectile-commander "projectile" "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods." t nil) (autoload 'projectile-browse-dirty-projects "projectile" "Browse dirty version controlled projects.

With a prefix argument, or if CACHED is non-nil, try to use the cached
dirty project list.

(fn &optional CACHED)" t nil) (autoload 'projectile-edit-dir-locals "projectile" "Edit or create a .dir-locals.el file of the project." t nil) (defvar projectile-mode nil "Non-nil if Projectile mode is enabled.
See the `projectile-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-mode'.") (custom-autoload 'projectile-mode "projectile" nil) (autoload 'projectile-mode "projectile" "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'projectile-global-mode 'projectile-mode "1.0") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "projectile" '("??" "compilation-find-file-projectile-find-compilation-buffer" "def-projectile-commander-method" "delete-file-projectile-remove-from-cache" "projectile-"))) (provide 'projectile-autoloads)) "key-chord" ((key-chord-autoloads key-chord) (defvar key-chord-mode nil "Non-nil if Key-Chord mode is enabled.
See the `key-chord-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `key-chord-mode'.") (custom-autoload 'key-chord-mode "key-chord" nil) (autoload 'key-chord-mode "key-chord" "Map pairs of simultaneously pressed keys to commands.

If called interactively, enable Key-Chord mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

See functions `key-chord-define-global', `key-chord-define-local',
and `key-chord-define' and variables `key-chord-two-keys-delay'
and `key-chord-one-key-delay'.

(fn &optional ARG)" t nil) (autoload 'key-chord-define-global "key-chord" "Define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

Note that KEYS defined locally in the current buffer will have
precedence.

(fn KEYS COMMAND)" t nil) (autoload 'key-chord-define-local "key-chord" "Locally define a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode.

(fn KEYS COMMAND)" t nil) (autoload 'key-chord-define "key-chord" "Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.

KEYS can be a string or a vector of two elements. Currently only
elements that corresponds to ascii codes in the range 32 to 126
can be used.

COMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

(fn KEYMAP KEYS COMMAND)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "key-chord" '("key-chord-"))) (provide 'key-chord-autoloads)) "company" ((company-ispell company-abbrev company company-oddmuse company-autoloads company-bbdb company-capf company-dabbrev company-clang company-elisp company-tempo company-yasnippet company-cmake company-etags company-gtags company-nxml company-files company-template company-keywords company-dabbrev-code company-tng company-semantic company-css) (autoload 'company-mode "company" "\"complete anything\"; is an in-buffer completion framework.
Completion starts automatically, depending on the values
`company-idle-delay' and `company-minimum-prefix-length'.

If called interactively, enable Company mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Completion can be controlled with the commands:
`company-complete-common', `company-complete-selection', `company-complete',
`company-select-next', `company-select-previous'.  If these commands are
called before `company-idle-delay', completion will also start.

Completions can be searched with `company-search-candidates' or
`company-filter-candidates'.  These can be used while completion is
inactive, as well.

The completion data is retrieved using `company-backends' and displayed
using `company-frontends'.  If you want to start a specific backend, call
it interactively or use `company-begin-backend'.

By default, the completions list is sorted alphabetically, unless the
backend chooses otherwise, or `company-transformers' changes it later.

regular keymap (`company-mode-map'):

\\{company-mode-map}
keymap during active completions (`company-active-map'):

\\{company-active-map}

(fn &optional ARG)" t nil) (put 'global-company-mode 'globalized-minor-mode t) (defvar global-company-mode nil "Non-nil if Global Company mode is enabled.
See the `global-company-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-mode'.") (custom-autoload 'global-company-mode "company" nil) (autoload 'global-company-mode "company" "Toggle Company mode in all buffers.
With prefix ARG, enable Global Company mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company mode is enabled in all buffers where
`company-mode-on' would do it.
See `company-mode' for more information on Company mode.

(fn &optional ARG)" t nil) (autoload 'company-manual-begin "company" nil t nil) (autoload 'company-complete "company" "Insert the common part of all candidates or the current selection.
The first time this is called, the common part is inserted, the second
time, or when the selection has been changed, the selected candidate is
inserted." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company" '("company-"))) (autoload 'company-abbrev "company-abbrev" "`company-mode' completion backend for abbrev.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-abbrev" '("company-abbrev-insert"))) (autoload 'company-bbdb "company-bbdb" "`company-mode' completion backend for BBDB.

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-bbdb" '("company-bbdb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-capf" '("company-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-clang" '("company-clang"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-cmake" '("company-cmake"))) (autoload 'company-css "company-css" "`company-mode' completion backend for `css-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-css" '("company-css-"))) (autoload 'company-dabbrev "company-dabbrev" "dabbrev-like `company-mode' completion backend.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev" '("company-dabbrev-"))) (autoload 'company-dabbrev-code "company-dabbrev-code" "dabbrev-like `company-mode' backend for code.
The backend looks for all symbols in the current buffer that aren't in
comments or strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-dabbrev-code" '("company-dabbrev-code-"))) (autoload 'company-elisp "company-elisp" "`company-mode' completion backend for Emacs Lisp.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-elisp" '("company-elisp-"))) (autoload 'company-etags "company-etags" "`company-mode' completion backend for etags.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-etags" '("company-etags-"))) (autoload 'company-files "company-files" "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-files" '("company-file"))) (autoload 'company-gtags "company-gtags" "`company-mode' completion backend for GNU Global.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-gtags" '("company-gtags-"))) (autoload 'company-ispell "company-ispell" "`company-mode' completion backend using Ispell.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ispell" '("company-ispell-"))) (autoload 'company-keywords "company-keywords" "`company-mode' backend for programming language keywords.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-keywords" '("company-keywords-"))) (autoload 'company-nxml "company-nxml" "`company-mode' completion backend for `nxml-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-nxml" '("company-nxml-"))) (autoload 'company-oddmuse "company-oddmuse" "`company-mode' completion backend for `oddmuse-mode'.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-oddmuse" '("company-oddmuse-"))) (autoload 'company-semantic "company-semantic" "`company-mode' completion backend using CEDET Semantic.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-semantic" '("company-semantic-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-template" '("company-template-"))) (autoload 'company-tempo "company-tempo" "`company-mode' completion backend for tempo.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tempo" '("company-tempo-"))) (autoload 'company-tng-frontend "company-tng" "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion.

(fn COMMAND)" nil nil) (define-obsolete-function-alias 'company-tng-configure-default 'company-tng-mode "0.9.14" "Applies the default configuration to enable company-tng.") (defvar company-tng-mode nil "Non-nil if Company-Tng mode is enabled.
See the `company-tng-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-tng-mode'.") (custom-autoload 'company-tng-mode "company-tng" nil) (autoload 'company-tng-mode "company-tng" "This minor mode enables `company-tng-frontend'.

If called interactively, enable Company-Tng mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-tng" '("company-tng-"))) (autoload 'company-yasnippet "company-yasnippet" "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook \\='js-mode-hook
            (lambda ()
              (set (make-local-variable \\='company-backends)
                   \\='((company-dabbrev-code company-yasnippet)))))

* After keyword `:with', grouped with other backends.

  (push \\='(company-semantic :with company-yasnippet) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") \\='company-yasnippet)

(fn COMMAND &optional ARG &rest IGNORE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-yasnippet" '("company-yasnippet-"))) (provide 'company-autoloads)) "company-c-headers" ((company-c-headers-autoloads company-c-headers) (autoload 'company-c-headers "company-c-headers" "Company backend for C/C++ header files.

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-c-headers" '("call-if-function" "company-c-headers-"))) (provide 'company-c-headers-autoloads)) "irony" ((irony-completion irony-cdb-json irony-cdb-clang-complete irony-autoloads irony-cdb-libclang irony-diagnostics \.dir-locals irony irony-snippet irony-iotask irony-cdb) (defvar irony-additional-clang-options nil "Additional command line options to pass down to libclang.

Please, do NOT use this variable to add header search paths, only
additional warnings or compiler options.

These compiler options will be prepended to the command line, in
order to not override the value coming from a compilation
database.") (custom-autoload 'irony-additional-clang-options "irony" t) (autoload 'irony-mode "irony" "Minor mode for C, C++ and Objective-C, powered by libclang.

If called interactively, enable Irony mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'irony-version "irony" "Return the version number of the file irony.el.

If called interactively display the version in the echo area.

(fn &optional SHOW-VERSION)" t nil) (autoload 'irony-server-kill "irony" "Kill the running irony-server process, if any." t nil) (autoload 'irony-get-type "irony" "Get the type of symbol under cursor." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony" '("irony-"))) (autoload 'irony-cdb-autosetup-compile-options "irony-cdb" nil t nil) (autoload 'irony-cdb-menu "irony-cdb" nil t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-cdb" '("irony-cdb-"))) (autoload 'irony-cdb-clang-complete "irony-cdb-clang-complete" "

(fn COMMAND &rest ARGS)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-cdb-clang-complete" '("irony-cdb-clang-complete--"))) (autoload 'irony-cdb-json "irony-cdb-json" "

(fn COMMAND &rest ARGS)" nil nil) (autoload 'irony-cdb-json-add-compile-commands-path "irony-cdb-json" "Add an out-of-source compilation database.

Files below the PROJECT-ROOT directory will use the JSON
Compilation Database as specified by COMPILE-COMMANDS-PATH.

The JSON Compilation Database are often generated in the build
directory. This functions helps mapping out-of-source build
directories to project directory.

(fn PROJECT-ROOT COMPILE-COMMANDS-PATH)" t nil) (autoload 'irony-cdb-json-select "irony-cdb-json" "Select CDB to use with a prompt.

It is useful when you have several CDBs with the same project
root.

The completion function used internally is `completing-read' so
it could easily be used with other completion functions by
temporarily using a let-bind on `completing-read-function'. Or
even helm by enabling `helm-mode' before calling the function." t nil) (autoload 'irony-cdb-json-select-most-recent "irony-cdb-json" "Select CDB that is most recently modified." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-cdb-json" '("irony-cdb-json--"))) (autoload 'irony-cdb-libclang "irony-cdb-libclang" "

(fn COMMAND &rest ARGS)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-cdb-libclang" '("irony-cdb-libclang--"))) (autoload 'irony-completion-at-point "irony-completion" nil nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-completion" '("irony-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-diagnostics" '("irony-diagnostics-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-iotask" '("irony-iotask-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "irony-snippet" '("irony-snippet-"))) (provide 'irony-autoloads)) "transient" ((transient-autoloads transient) (autoload 'transient-insert-suffix "transient" "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-insert-suffix 'lisp-indent-function 'defun) (autoload 'transient-append-suffix "transient" "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-append-suffix 'lisp-indent-function 'defun) (autoload 'transient-replace-suffix "transient" "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC SUFFIX)" nil nil) (function-put 'transient-replace-suffix 'lisp-indent-function 'defun) (autoload 'transient-remove-suffix "transient" "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'.

(fn PREFIX LOC)" nil nil) (function-put 'transient-remove-suffix 'lisp-indent-function 'defun) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "transient" '("magit--fit-window-to-buffer" "transient-"))) (provide 'transient-autoloads)) "with-editor" ((with-editor-autoloads with-editor) (autoload 'with-editor-export-editor "with-editor" "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode', `eshell-mode' and
`vterm'.

(fn &optional (ENVVAR \"EDITOR\"))" t nil) (autoload 'with-editor-export-git-editor "with-editor" "Like `with-editor-export-editor' but always set `$GIT_EDITOR'." t nil) (autoload 'with-editor-export-hg-editor "with-editor" "Like `with-editor-export-editor' but always set `$HG_EDITOR'." t nil) (defvar shell-command-with-editor-mode nil "Non-nil if Shell-Command-With-Editor mode is enabled.
See the `shell-command-with-editor-mode' command
for a description of this minor mode.") (custom-autoload 'shell-command-with-editor-mode "with-editor" nil) (autoload 'shell-command-with-editor-mode "with-editor" "Teach `shell-command' to use current Emacs instance as editor.

If called interactively, enable Shell-Command-With-Editor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\".

(fn &optional ARG)" t nil) (autoload 'with-editor-async-shell-command "with-editor" "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (autoload 'with-editor-shell-command "with-editor" "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former.

(fn COMMAND &optional OUTPUT-BUFFER ERROR-BUFFER ENVVAR)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "with-editor" '("server-" "shell-command--shell-command-with-editor-mode" "start-file-process--with-editor-process-filter" "with-editor"))) (provide 'with-editor-autoloads)) "git-commit" ((git-commit-autoloads git-commit-pkg git-commit) (put 'git-commit-major-mode 'safe-local-variable (lambda (val) (memq val '(text-mode markdown-mode org-mode fundamental-mode git-commit-elisp-text-mode)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode"))) (provide 'git-commit-autoloads)) "magit-section" ((magit-section-pkg magit-section-autoloads magit-section) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-section" '("isearch-clean-overlays@magit-mode" "magit-"))) (provide 'magit-section-autoloads)) "magit" ((magit-bookmark magit-gitignore magit-pkg magit-status magit-reset magit-transient magit-worktree magit-margin magit-tag magit-repos magit-autoloads magit-diff magit-branch magit-apply magit-commit git-rebase magit-blame magit-process magit-mode magit-ediff magit-pull magit-sequence magit-fetch magit-notes magit-merge magit-utils magit-submodule magit-push magit-refs magit-remote magit-subtree magit magit-core magit-log magit-bundle magit-obsolete magit-extras magit-stash magit-reflog magit-wip magit-patch magit-git magit-autorevert magit-imenu magit-clone magit-files magit-bisect) (autoload 'git-rebase-current-line "git-rebase" "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned." nil nil) (autoload 'git-rebase-mode "git-rebase" "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

(fn)" t nil) (defconst git-rebase-filename-regexp "/git-rebase-todo\\'") (add-to-list 'auto-mode-alist (cons git-rebase-filename-regexp 'git-rebase-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-rebase" '("git-rebase-"))) (define-obsolete-variable-alias 'global-magit-file-mode 'magit-define-global-key-bindings "Magit 3.0.0") (defvar magit-define-global-key-bindings t "Whether to bind some Magit commands in the global keymap.

If this variable is non-nil, then the following bindings may
be added to the global keymap.  The default is t.

key             binding
---             -------
C-x g           magit-status
C-x M-g         magit-dispatch
C-c M-g         magit-file-dispatch

These bindings may be added when `after-init-hook' is run.
Each binding is added if and only if at that time no other key
is bound to the same command and no other command is bound to
the same key.  In other words we try to avoid adding bindings
that are unnecessary, as well as bindings that conflict with
other bindings.

Adding the above bindings is delayed until `after-init-hook'
is called to allow users to set the variable anywhere in their
init file (without having to make sure to do so before `magit'
is loaded or autoloaded) and to increase the likelihood that
all the potentially conflicting user bindings have already
been added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable to nil has no effect if that is done after
the key bindings have already been added.

We recommend that you bind \"C-c g\" instead of \"C-c M-g\" to
`magit-file-dispatch'.  The former is a much better binding
but the \"C-c <letter>\" namespace is strictly reserved for
users; preventing Magit from using it by default.

Also see info node `(magit)Commands for Buffers Visiting Files'.") (custom-autoload 'magit-define-global-key-bindings "magit" t) (defun magit-maybe-define-global-key-bindings nil (when magit-define-global-key-bindings (let ((map (current-global-map))) (dolist (elt '(("C-x g" . magit-status) ("C-x M-g" . magit-dispatch) ("C-c M-g" . magit-file-dispatch))) (let ((key (kbd (car elt))) (def (cdr elt))) (unless (or (lookup-key map key) (where-is-internal def (make-sparse-keymap) t)) (define-key map key def))))))) (if after-init-time (magit-maybe-define-global-key-bindings) (add-hook 'after-init-hook 'magit-maybe-define-global-key-bindings t)) (autoload 'magit-dispatch "magit" nil t) (autoload 'magit-run "magit" nil t) (autoload 'magit-git-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-git-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree.

(fn COMMAND)" t nil) (autoload 'magit-shell-command "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'.

(fn COMMAND)" t nil) (autoload 'magit-shell-command-topdir "magit" "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree.

(fn COMMAND)" t nil) (autoload 'magit-version "magit" "Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it.

(fn &optional PRINT-DEST)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit" '("magit-"))) (autoload 'magit-stage-file "magit-apply" "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-stage-modified "magit-apply" "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.

(fn &optional ALL)" t nil) (autoload 'magit-unstage-file "magit-apply" "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation.

(fn FILE)" t nil) (autoload 'magit-unstage-all "magit-apply" "Remove all changes from the staging area." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-apply" '("magit-"))) (put 'magit-auto-revert-mode 'globalized-minor-mode t) (defvar magit-auto-revert-mode (not (or global-auto-revert-mode noninteractive)) "Non-nil if Magit-Auto-Revert mode is enabled.
See the `magit-auto-revert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-auto-revert-mode'.") (custom-autoload 'magit-auto-revert-mode "magit-autorevert" nil) (autoload 'magit-auto-revert-mode "magit-autorevert" "Toggle Auto-Revert mode in all buffers.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Auto-Revert mode is enabled in all buffers where
`magit-turn-on-auto-revert-mode-if-desired' would do it.
See `auto-revert-mode' for more information on Auto-Revert mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-autorevert" '("auto-revert-buffer" "magit-"))) (autoload 'magit-bisect "magit-bisect" nil t) (autoload 'magit-bisect-start "magit-bisect" "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\\<magit-status-mode-map>\\[magit-bisect]).

(fn BAD GOOD ARGS)" t nil) (autoload 'magit-bisect-reset "magit-bisect" "After bisecting, cleanup bisection state and return to original `HEAD'." t nil) (autoload 'magit-bisect-good "magit-bisect" "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question." t nil) (autoload 'magit-bisect-bad "magit-bisect" "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question." t nil) (autoload 'magit-bisect-mark "magit-bisect" "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms." t nil) (autoload 'magit-bisect-skip "magit-bisect" "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one." t nil) (autoload 'magit-bisect-run "magit-bisect" "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'.

(fn CMDLINE &optional BAD GOOD ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bisect" '("magit-"))) (autoload 'magit-blame-echo "magit-blame" nil t) (autoload 'magit-blame-addition "magit-blame" nil t) (autoload 'magit-blame-removal "magit-blame" nil t) (autoload 'magit-blame-reverse "magit-blame" nil t) (autoload 'magit-blame "magit-blame" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-blame" '("magit-"))) (autoload 'magit--handle-bookmark "magit-bookmark" "Open a bookmark created by `magit--make-bookmark'.
Call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.  Ignore `magit-display-buffer-function'.

(fn BOOKMARK)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bookmark" '("magit--make-bookmark"))) (autoload 'magit-branch "magit" nil t) (autoload 'magit-checkout "magit-branch" "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.

(git checkout REVISION).

(fn REVISION &optional ARGS)" t nil) (autoload 'magit-branch-create "magit-branch" "Create BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-and-checkout "magit-branch" "Create and checkout BRANCH at branch or revision START-POINT.

(fn BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-branch-or-checkout "magit-branch" "Hybrid between `magit-checkout' and `magit-branch-and-checkout'.

Ask the user for an existing branch or revision.  If the user
input actually can be resolved as a branch or revision, then
check that out, just like `magit-checkout' would.

Otherwise create and checkout a new branch using the input as
its name.  Before doing so read the starting-point for the new
branch.  This is similar to what `magit-branch-and-checkout'
does.

(fn ARG &optional START-POINT)" t nil) (autoload 'magit-branch-checkout "magit-branch" "Checkout an existing or new local branch.

Read a branch name from the user offering all local branches and
a subset of remote branches as candidates.  Omit remote branches
for which a local branch by the same name exists from the list
of candidates.  The user can also enter a completely new branch
name.

- If the user selects an existing local branch, then check that
  out.

- If the user selects a remote branch, then create and checkout
  a new local branch with the same name.  Configure the selected
  remote branch as push target.

- If the user enters a new branch name, then create and check
  that out, after also reading the starting-point from the user.

In the latter two cases the upstream is also set.  Whether it is
set to the chosen START-POINT or something else depends on the
value of `magit-branch-adjust-remote-upstream-alist', just like
when using `magit-branch-and-checkout'.

(fn BRANCH &optional START-POINT)" t nil) (autoload 'magit-branch-orphan "magit-branch" "Create and checkout an orphan BRANCH with contents from revision START-POINT.

(fn BRANCH START-POINT)" t nil) (autoload 'magit-branch-spinout "magit-branch" "Create new branch from the unpushed commits.
Like `magit-branch-spinoff' but remain on the current branch.
If there are any uncommitted changes, then behave exactly like
`magit-branch-spinoff'.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-spinoff "magit-branch" "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised.

(fn BRANCH &optional FROM)" t nil) (autoload 'magit-branch-reset "magit-branch" "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset.

(fn BRANCH TO &optional SET-UPSTREAM)" t nil) (autoload 'magit-branch-delete "magit-branch" "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point.

(fn BRANCHES &optional FORCE)" t nil) (autoload 'magit-branch-rename "magit-branch" "Rename the branch named OLD to NEW.

With a prefix argument FORCE, rename even if a branch named NEW
already exists.

If `branch.OLD.pushRemote' is set, then unset it.  Depending on
the value of `magit-branch-rename-push-target' (which see) maybe
set `branch.NEW.pushRemote' and maybe rename the push-target on
the remote.

(fn OLD NEW &optional FORCE)" t nil) (autoload 'magit-branch-shelve "magit-branch" "Shelve a BRANCH.
Rename \"refs/heads/BRANCH\" to \"refs/shelved/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-unshelve "magit-branch" "Unshelve a BRANCH
Rename \"refs/shelved/BRANCH\" to \"refs/heads/BRANCH\",
and also rename the respective reflog file.

(fn BRANCH)" t nil) (autoload 'magit-branch-configure "magit-branch" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-branch" '("magit-"))) (autoload 'magit-bundle "magit-bundle" nil t) (autoload 'magit-bundle-import "magit-bundle" nil t) (autoload 'magit-bundle-create-tracked "magit-bundle" "Create and track a new bundle.

(fn FILE TAG BRANCH REFS ARGS)" t nil) (autoload 'magit-bundle-update-tracked "magit-bundle" "Update a bundle that is being tracked using TAG.

(fn TAG)" t nil) (autoload 'magit-bundle-verify "magit-bundle" "Check whether FILE is valid and applies to the current repository.

(fn FILE)" t nil) (autoload 'magit-bundle-list-heads "magit-bundle" "List the refs in FILE.

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-bundle" '("magit-"))) (autoload 'magit-clone "magit-clone" nil t) (autoload 'magit-clone-regular "magit-clone" "Create a clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-shallow "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
With a prefix argument read the DEPTH of the clone;
otherwise use 1.

(fn REPOSITORY DIRECTORY ARGS DEPTH)" t nil) (autoload 'magit-clone-shallow-since "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits before DATE, which is read from the
user.

(fn REPOSITORY DIRECTORY ARGS DATE)" t nil) (autoload 'magit-clone-shallow-exclude "magit-clone" "Create a shallow clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.
Exclude commits reachable from EXCLUDE, which is a
branch or tag read from the user.

(fn REPOSITORY DIRECTORY ARGS EXCLUDE)" t nil) (autoload 'magit-clone-bare "magit-clone" "Create a bare clone of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (autoload 'magit-clone-mirror "magit-clone" "Create a mirror of REPOSITORY in DIRECTORY.
Then show the status buffer for the new repository.

(fn REPOSITORY DIRECTORY ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-clone" '("magit-clone-"))) (autoload 'magit-commit "magit-commit" nil t) (autoload 'magit-commit-create "magit-commit" "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.

(git commit [--amend] ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-amend "magit-commit" "Amend the last commit.

(git commit --amend ARGS)

(fn &optional ARGS)" t nil) (autoload 'magit-commit-extend "magit-commit" "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  
(git commit
--amend --no-edit)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-reword "magit-commit" "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.

(git commit --amend --only)

(fn &optional ARGS OVERRIDE-DATE)" t nil) (autoload 'magit-commit-fixup "magit-commit" "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-squash "magit-commit" "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

If you want to immediately add a message to the squash commit,
then use `magit-commit-augment' instead of this command.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-augment "magit-commit" "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-fixup "magit-commit" "Create a fixup commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-instant-squash "magit-commit" "Create a squash commit targeting COMMIT and instantly rebase.

(fn &optional COMMIT ARGS)" t nil) (autoload 'magit-commit-reshelve "magit-commit" "Change the committer date and possibly the author date of `HEAD'.

The current time is used as the initial minibuffer input and the
original author or committer date is available as the previous
history element.

Both the author and the committer dates are changes, unless one
of the following is true, in which case only the committer date
is updated:
- You are not the author of the commit that is being reshelved.
- The command was invoked with a prefix argument.
- Non-interactively if UPDATE-AUTHOR is nil.

(fn DATE UPDATE-AUTHOR &optional ARGS)" t nil) (autoload 'magit-commit-absorb-modules "magit-commit" "Spread modified modules across recent commits.

(fn PHASE COMMIT)" t nil) (autoload 'magit-commit-absorb "magit-commit" nil t) (autoload 'magit-commit-autofixup "magit-commit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-commit" '("magit-"))) (autoload 'magit-diff "magit-diff" nil t) (autoload 'magit-diff-refresh "magit-diff" nil t) (autoload 'magit-diff-dwim "magit-diff" "Show changes for the thing at point.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-range "magit-diff" "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range).

(fn REV-OR-RANGE &optional ARGS FILES)" t nil) (autoload 'magit-diff-working-tree "magit-diff" "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-staged "magit-diff" "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer.

(fn &optional REV ARGS FILES)" t nil) (autoload 'magit-diff-unstaged "magit-diff" "Show changes between the working tree and the index.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-unmerged "magit-diff" "Show changes that are being merged.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-diff-while-committing "magit-diff" "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed.

(fn &optional ARGS)" t nil) (autoload 'magit-diff-buffer-file "magit-diff" "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differenced between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob." t nil) (autoload 'magit-diff-paths "magit-diff" "Show changes between any two files on disk.

(fn A B)" t nil) (autoload 'magit-show-commit "magit-diff" "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision.

(fn REV &optional ARGS FILES MODULE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-diff" '("magit-"))) (autoload 'magit-ediff "magit-ediff" nil) (autoload 'magit-ediff-resolve "magit-ediff" "Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

(fn FILE)" t nil) (autoload 'magit-ediff-stage "magit-ediff" "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-compare "magit-ediff" "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil, then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range).

(fn REVA REVB FILEA FILEB)" t nil) (autoload 'magit-ediff-dwim "magit-ediff" "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t nil) (autoload 'magit-ediff-show-staged "magit-ediff" "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-unstaged "magit-ediff" "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-working-tree "magit-ediff" "Show changes between `HEAD' and working tree using Ediff.
FILE must be relative to the top directory of the repository.

(fn FILE)" t nil) (autoload 'magit-ediff-show-commit "magit-ediff" "Show changes introduced by COMMIT using Ediff.

(fn COMMIT)" t nil) (autoload 'magit-ediff-show-stash "magit-ediff" "Show changes introduced by STASH using Ediff.
`magit-ediff-show-stash-with-index' controls whether a
three-buffer Ediff is used in order to distinguish changes in the
stash that were staged.

(fn STASH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-ediff" '("magit-ediff-"))) (autoload 'magit-run-git-gui "magit-extras" "Run `git gui' for the current git repository." t nil) (autoload 'magit-run-git-gui-blame "magit-extras" "Run `git gui blame' on the given FILENAME and COMMIT.
Interactively run it for the current file and the `HEAD', with a
prefix or when the current file cannot be determined let the user
choose.  When the current buffer is visiting FILENAME instruct
blame to center around the line point is on.

(fn COMMIT FILENAME &optional LINENUM)" t nil) (autoload 'magit-run-gitk "magit-extras" "Run `gitk' in the current repository." t nil) (autoload 'magit-run-gitk-branches "magit-extras" "Run `gitk --branches' in the current repository." t nil) (autoload 'magit-run-gitk-all "magit-extras" "Run `gitk --all' in the current repository." t nil) (autoload 'ido-enter-magit-status "magit-extras" "Drop into `magit-status' from file switching.

This command does not work in Emacs 26.1.
See https://github.com/magit/magit/issues/3634
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31707.

To make this command available use something like:

  (add-hook \\='ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") \\='ido-enter-magit-status)))

Starting with Emacs 25.1 the Ido keymaps are defined just once
instead of every time Ido is invoked, so now you can modify it
like pretty much every other keymap:

  (define-key ido-common-completion-map
    (kbd \"C-x g\") \\='ido-enter-magit-status)" t nil) (autoload 'magit-project-status "magit-extras" "Run `magit-status' in the current project's root." t nil) (autoload 'magit-dired-jump "magit-extras" "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'.

(fn &optional OTHER-WINDOW)" t nil) (autoload 'magit-dired-log "magit-extras" "Show log for all marked files, or the current file.

(fn &optional FOLLOW)" t nil) (autoload 'magit-dired-am-apply-patches "magit-extras" "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository.

(fn REPO &optional ARG)" t nil) (autoload 'magit-do-async-shell-command "magit-extras" "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point.

(fn FILE)" t nil) (autoload 'magit-previous-line "magit-extras" "Like `previous-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects an
area that is larger than the region.  This causes `previous-line'
when invoked while holding the shift key to move up one line and
thereby select two lines.  When invoked inside a hunk body this
command does not move point on the first invocation and thereby
it only selects a single line.  Which inconsistency you prefer
is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-previous-line 'interactive-only '"use `forward-line' with negative argument instead.") (autoload 'magit-next-line "magit-extras" "Like `next-line' but with Magit-specific shift-selection.

Magit's selection mechanism is based on the region but selects
an area that is larger than the region.  This causes `next-line'
when invoked while holding the shift key to move down one line
and thereby select two lines.  When invoked inside a hunk body
this command does not move point on the first invocation and
thereby it only selects a single line.  Which inconsistency you
prefer is a matter of preference.

(fn &optional ARG TRY-VSCROLL)" t nil) (function-put 'magit-next-line 'interactive-only 'forward-line) (autoload 'magit-clean "magit-extras" "Remove untracked files from the working tree.
With a prefix argument also remove ignored files,
with two prefix arguments remove ignored files only.

(git clean -f -d [-x|-X])

(fn &optional ARG)" t nil) (autoload 'magit-add-change-log-entry "magit-extras" "Find change log file and add date entry and item for current change.
This differs from `add-change-log-entry' (which see) in that
it acts on the current hunk in a Magit buffer instead of on
a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME OTHER-WINDOW)" t nil) (autoload 'magit-add-change-log-entry-other-window "magit-extras" "Find change log file in other window and add entry and item.
This differs from `add-change-log-entry-other-window' (which see)
in that it acts on the current hunk in a Magit buffer instead of
on a position in a file-visiting buffer.

(fn &optional WHOAMI FILE-NAME)" t nil) (autoload 'magit-edit-line-commit "magit-extras" "Edit the commit that added the current line.

With a prefix argument edit the commit that removes the line,
if any.  The commit is determined using `git blame' and made
editable using `git rebase --interactive' if it is reachable
from `HEAD', or by checking out the commit (or a branch that
points at it) otherwise.

(fn &optional TYPE)" t nil) (autoload 'magit-diff-edit-hunk-commit "magit-extras" "From a hunk, edit the respective commit and visit the file.

First visit the file being modified by the hunk at the correct
location using `magit-diff-visit-file'.  This actually visits a
blob.  When point is on a diff header, not within an individual
hunk, then this visits the blob the first hunk is about.

Then invoke `magit-edit-line-commit', which uses an interactive
rebase to make the commit editable, or if that is not possible
because the commit is not reachable from `HEAD' by checking out
that commit directly.  This also causes the actual worktree file
to be visited.

Neither the blob nor the file buffer are killed when finishing
the rebase.  If that is undesirable, then it might be better to
use `magit-rebase-edit-command' instead of this command.

(fn FILE)" t nil) (autoload 'magit-reshelve-since "magit-extras" "Change the author and committer dates of the commits since REV.

Ask the user for the first reachable commit whose dates should
be changed.  Then read the new date for that commit.  The initial
minibuffer input and the previous history element offer good
values.  The next commit will be created one minute later and so
on.

This command is only intended for interactive use and should only
be used on highly rearranged and unpublished history.

If KEYID is non-nil, then use that to sign all reshelved commits.
Interactively use the value of the \"--gpg-sign\" option in the
list returned by `magit-rebase-arguments'.

(fn REV KEYID)" t nil) (autoload 'magit-pop-revision-stack "magit-extras" "Insert a representation of a revision into the current buffer.

Pop a revision from the `magit-revision-stack' and insert it into
the current buffer according to `magit-pop-revision-stack-format'.
Revisions can be put on the stack using `magit-copy-section-value'
and `magit-copy-buffer-revision'.

If the stack is empty or with a prefix argument, instead read a
revision in the minibuffer.  By using the minibuffer history this
allows selecting an item which was popped earlier or to insert an
arbitrary reference or revision without first pushing it onto the
stack.

When reading the revision from the minibuffer, then it might not
be possible to guess the correct repository.  When this command
is called inside a repository (e.g. while composing a commit
message), then that repository is used.  Otherwise (e.g. while
composing an email) then the repository recorded for the top
element of the stack is used (even though we insert another
revision).  If not called inside a repository and with an empty
stack, or with two prefix arguments, then read the repository in
the minibuffer too.

(fn REV TOPLEVEL)" t nil) (autoload 'magit-copy-section-value "magit-extras" "Save the value of the current section for later use.

Save the section value to the `kill-ring', and, provided that
the current section is a commit, branch, or tag section, push
the (referenced) revision to the `magit-revision-stack' for use
with `magit-pop-revision-stack'.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'.

When the current section is a branch or a tag, and a prefix
argument is used, then save the revision at its tip to the
`kill-ring' instead of the reference name.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.  If a prefix argument is used and the region is within
a hunk, then strip the diff marker column and keep only either
the added or removed lines, depending on the sign of the prefix
argument.

(fn ARG)" t nil) (autoload 'magit-copy-buffer-revision "magit-extras" "Save the revision of the current buffer for later use.

Save the revision shown in the current buffer to the `kill-ring'
and push it to the `magit-revision-stack'.

This command is mainly intended for use in `magit-revision-mode'
buffers, the only buffers where it is always unambiguous exactly
which revision should be saved.

Most other Magit buffers usually show more than one revision, in
some way or another, so this command has to select one of them,
and that choice might not always be the one you think would have
been the best pick.

In such buffers it is often more useful to save the value of
the current section instead, using `magit-copy-section-value'.

When the region is active, then save that to the `kill-ring',
like `kill-ring-save' would, instead of behaving as described
above.

When `magit-copy-revision-abbreviated' is non-nil, save the
abbreviated revision to the `kill-ring' and the
`magit-revision-stack'." t nil) (autoload 'magit-display-repository-buffer "magit-extras" "Display a Magit buffer belonging to the current Git repository.
The buffer is displayed using `magit-display-buffer', which see.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer-other-window "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-switch-to-repository-buffer-other-frame "magit-extras" "Switch to a Magit buffer belonging to the current Git repository.

(fn BUFFER)" t nil) (autoload 'magit-abort-dwim "magit-extras" "Abort current operation.
Depending on the context, this will abort a merge, a rebase, a
patch application, a cherry-pick, a revert, or a bisect." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-extras" '("magit-"))) (autoload 'magit-fetch "magit-fetch" nil t) (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t) (autoload 'magit-fetch-from-upstream "magit-fetch" nil t) (autoload 'magit-fetch-other "magit-fetch" "Fetch from another repository.

(fn REMOTE ARGS)" t nil) (autoload 'magit-fetch-branch "magit-fetch" "Fetch a BRANCH from a REMOTE.

(fn REMOTE BRANCH ARGS)" t nil) (autoload 'magit-fetch-refspec "magit-fetch" "Fetch a REFSPEC from a REMOTE.

(fn REMOTE REFSPEC ARGS)" t nil) (autoload 'magit-fetch-all "magit-fetch" "Fetch from all remotes.

(fn ARGS)" t nil) (autoload 'magit-fetch-all-prune "magit-fetch" "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote." t nil) (autoload 'magit-fetch-all-no-prune "magit-fetch" "Fetch from all remotes." t nil) (autoload 'magit-fetch-modules "magit-fetch" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-fetch" '("magit-"))) (autoload 'magit-find-file "magit-files" "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-window "magit-files" "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-find-file-other-frame "magit-files" "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location.

(fn REV FILE)" t nil) (autoload 'magit-file-dispatch "magit" nil t) (autoload 'magit-blob-visit-file "magit-files" "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree." t nil) (autoload 'magit-file-checkout "magit-files" "Checkout FILE from REV.

(fn REV FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-files" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-git" '("magit-"))) (autoload 'magit-gitignore "magit-gitignore" nil t) (autoload 'magit-gitignore-in-topdir "magit-gitignore" "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file.

(fn RULE)" t nil) (autoload 'magit-gitignore-in-subdir "magit-gitignore" "Add the Git ignore RULE to a \".gitignore\" file in DIRECTORY.
Prompt the user for a directory and add the rule to the
\".gitignore\" file in that directory.  Since such files are
tracked, they are shared with other clones of the repository.
Also stage the file.

(fn RULE DIRECTORY)" t nil) (autoload 'magit-gitignore-in-gitdir "magit-gitignore" "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository.

(fn RULE)" t nil) (autoload 'magit-gitignore-on-system "magit-gitignore" "Add the Git ignore RULE to the file specified by `core.excludesFile'.
Rules that are defined in that file affect all local repositories.

(fn RULE)" t nil) (autoload 'magit-skip-worktree "magit-gitignore" "Call \"git update-index --skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-skip-worktree "magit-gitignore" "Call \"git update-index --no-skip-worktree -- FILE\".

(fn FILE)" t nil) (autoload 'magit-assume-unchanged "magit-gitignore" "Call \"git update-index --assume-unchanged -- FILE\".

(fn FILE)" t nil) (autoload 'magit-no-assume-unchanged "magit-gitignore" "Call \"git update-index --no-assume-unchanged -- FILE\".

(fn FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-gitignore" '("magit-"))) (autoload 'magit-imenu--log-prev-index-position-function "magit-imenu" "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--log-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--diff-prev-index-position-function "magit-imenu" "Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--diff-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--status-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--refs-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--cherry-create-index-function "magit-imenu" "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'." nil nil) (autoload 'magit-imenu--submodule-prev-index-position-function "magit-imenu" "Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--submodule-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--repolist-prev-index-position-function "magit-imenu" "Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--repolist-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--process-prev-index-position-function "magit-imenu" "Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--process-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (autoload 'magit-imenu--rebase-prev-index-position-function "magit-imenu" "Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'." nil nil) (autoload 'magit-imenu--rebase-extract-index-name-function "magit-imenu" "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-imenu" '("magit-imenu--index-function"))) (autoload 'magit-log "magit-log" nil t) (autoload 'magit-log-refresh "magit-log" nil t) (autoload 'magit-log-current "magit-log" "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-other "magit-log" "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates.

(fn REVS &optional ARGS FILES)" t nil) (autoload 'magit-log-head "magit-log" "Show log for `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-branches "magit-log" "Show log for all local branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-branches "magit-log" "Show log for all branches matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-matching-tags "magit-log" "Show log for all tags matching PATTERN and `HEAD'.

(fn PATTERN &optional ARGS FILES)" t nil) (autoload 'magit-log-all-branches "magit-log" "Show log for all local and remote branches and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-all "magit-log" "Show log for all references and `HEAD'.

(fn &optional ARGS FILES)" t nil) (autoload 'magit-log-buffer-file "magit-log" "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is an active log
argument, then follow renames.  When the region is active,
restrict the log to the lines that the region touches.

(fn &optional FOLLOW BEG END)" t nil) (autoload 'magit-log-trace-definition "magit-log" "Show log for the definition at point.

(fn FILE FN REV)" t nil) (autoload 'magit-log-merged "magit-log" "Show log for the merge of COMMIT into BRANCH.

More precisely, find merge commit M that brought COMMIT into
BRANCH, and show the log of the range \"M^1..M\".  If COMMIT is
directly on BRANCH, then show approximately twenty surrounding
commits instead.

This command requires git-when-merged, which is available from
https://github.com/mhagger/git-when-merged.

(fn COMMIT BRANCH &optional ARGS FILES)" t nil) (autoload 'magit-log-move-to-parent "magit-log" "Move to the Nth parent of the current commit.

(fn &optional N)" t nil) (autoload 'magit-shortlog "magit-log" nil t) (autoload 'magit-shortlog-since "magit-log" "Show a history summary for commits since REV.

(fn REV ARGS)" t nil) (autoload 'magit-shortlog-range "magit-log" "Show a history summary for commit or range REV-OR-RANGE.

(fn REV-OR-RANGE ARGS)" t nil) (autoload 'magit-cherry "magit-log" "Show commits in a branch that are not merged in the upstream branch.

(fn HEAD UPSTREAM)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-log" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-margin" '("magit-"))) (autoload 'magit-merge "magit" nil t) (autoload 'magit-merge-plain "magit-merge" "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

(git merge --no-edit|--no-commit [ARGS] REV)

(fn REV &optional ARGS NOCOMMIT)" t nil) (autoload 'magit-merge-editmsg "magit-merge" "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.

(git merge --edit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-nocommit "magit-merge" "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.

(git merge --no-commit --no-ff [ARGS] REV)

(fn REV &optional ARGS)" t nil) (autoload 'magit-merge-into "magit-merge" "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-absorb "magit-merge" "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `forge-branch-pullreq' was used to create the merged branch,
then also remove the respective remote branch.

(fn BRANCH &optional ARGS)" t nil) (autoload 'magit-merge-squash "magit-merge" "Squash commit REV into the current branch; don't create a commit.

(git merge --squash REV)

(fn REV)" t nil) (autoload 'magit-merge-preview "magit-merge" "Preview result of merging REV into the current branch.

(fn REV)" t nil) (autoload 'magit-merge-abort "magit-merge" "Abort the current merge operation.

(git merge --abort)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-merge" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-mode" '("disable-magit-save-buffers" "magit-"))) (autoload 'magit-notes "magit" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-notes" '("magit-notes-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-obsolete" '("magit--magit-popup-warning"))) (autoload 'magit-patch "magit-patch" nil t) (autoload 'magit-patch-create "magit-patch" nil t) (autoload 'magit-patch-apply "magit-patch" nil t) (autoload 'magit-patch-save "magit-patch" "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used.

(fn FILE &optional ARG)" t nil) (autoload 'magit-request-pull "magit-patch" "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit.

(fn URL START END)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-patch" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-process" '("magit-" "tramp-sh-handle-"))) (autoload 'magit-pull "magit-pull" nil t) (autoload 'magit-pull-from-pushremote "magit-pull" nil t) (autoload 'magit-pull-from-upstream "magit-pull" nil t) (autoload 'magit-pull-branch "magit-pull" "Pull from a branch read in the minibuffer.

(fn SOURCE ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-pull" '("magit-pull-"))) (autoload 'magit-push "magit-push" nil t) (autoload 'magit-push-current-to-pushremote "magit-push" nil t) (autoload 'magit-push-current-to-upstream "magit-push" nil t) (autoload 'magit-push-current "magit-push" "Push the current branch to a branch read in the minibuffer.

(fn TARGET ARGS)" t nil) (autoload 'magit-push-other "magit-push" "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer.

(fn SOURCE TARGET ARGS)" t nil) (autoload 'magit-push-refspecs "magit-push" "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used.

(fn REMOTE REFSPECS ARGS)" t nil) (autoload 'magit-push-matching "magit-push" "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tags "magit-push" "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default.

(fn REMOTE &optional ARGS)" t nil) (autoload 'magit-push-tag "magit-push" "Push a tag to another repository.

(fn TAG REMOTE &optional ARGS)" t nil) (autoload 'magit-push-notes-ref "magit-push" "Push a notes ref to another repository.

(fn REF REMOTE &optional ARGS)" t nil) (autoload 'magit-push-implicitly "magit-push" nil t) (autoload 'magit-push-to-remote "magit-push" "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

(fn REMOTE ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-push" '("magit-"))) (autoload 'magit-reflog-current "magit-reflog" "Display the reflog of the current branch.
If `HEAD' is detached, then show the reflog for that instead." t nil) (autoload 'magit-reflog-other "magit-reflog" "Display the reflog of a branch or another ref.

(fn REF)" t nil) (autoload 'magit-reflog-head "magit-reflog" "Display the `HEAD' reflog." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reflog" '("magit-reflog-"))) (autoload 'magit-show-refs "magit-refs" nil t) (autoload 'magit-show-refs-head "magit-refs" "List and compare references in a dedicated buffer.
Compared with `HEAD'.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-current "magit-refs" "List and compare references in a dedicated buffer.
Compare with the current branch or `HEAD' if it is detached.

(fn &optional ARGS)" t nil) (autoload 'magit-show-refs-other "magit-refs" "List and compare references in a dedicated buffer.
Compared with a branch read from the user.

(fn &optional REF ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-refs" '("magit-"))) (autoload 'magit-remote "magit-remote" nil t) (autoload 'magit-remote-add "magit-remote" "Add a remote named REMOTE and fetch it.

(fn REMOTE URL &optional ARGS)" t nil) (autoload 'magit-remote-rename "magit-remote" "Rename the remote named OLD to NEW.

(fn OLD NEW)" t nil) (autoload 'magit-remote-remove "magit-remote" "Delete the remote named REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune "magit-remote" "Remove stale remote-tracking branches for REMOTE.

(fn REMOTE)" t nil) (autoload 'magit-remote-prune-refspecs "magit-remote" "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed.

(fn REMOTE)" t nil) (autoload 'magit-remote-set-head "magit-remote" "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that.

(fn REMOTE &optional BRANCH)" t nil) (autoload 'magit-remote-unset-head "magit-remote" "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\".

(fn REMOTE)" t nil) (autoload 'magit-remote-unshallow "magit-remote" "Convert a shallow remote into a full one.
If only a single refspec is set and it does not contain a
wildcard, then also offer to replace it with the standard
refspec.

(fn REMOTE)" t nil) (autoload 'magit-remote-configure "magit-remote" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-remote" '("magit-"))) (autoload 'magit-list-repositories "magit-repos" "Display a list of repositories.

Use the options `magit-repository-directories' to control which
repositories are displayed." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-repos" '("magit-"))) (autoload 'magit-reset "magit" nil t) (autoload 'magit-reset-mixed "magit-reset" "Reset the `HEAD' and index to COMMIT, but not the working tree.

(git reset --mixed COMMIT)

(fn COMMIT)" t nil) (autoload 'magit-reset-soft "magit-reset" "Reset the `HEAD' to COMMIT, but not the index and working tree.

(git reset --soft REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-hard "magit-reset" "Reset the `HEAD', index, and working tree to COMMIT.

(git reset --hard REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-keep "magit-reset" "Reset the `HEAD' and index to COMMIT, while keeping uncommitted changes.

(git reset --keep REVISION)

(fn COMMIT)" t nil) (autoload 'magit-reset-index "magit-reset" "Reset the index to COMMIT.
Keep the `HEAD' and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.

(git reset COMMIT .)

(fn COMMIT)" t nil) (autoload 'magit-reset-worktree "magit-reset" "Reset the worktree to COMMIT.
Keep the `HEAD' and index as-is.

(fn COMMIT)" t nil) (autoload 'magit-reset-quickly "magit-reset" "Reset the `HEAD' and index to COMMIT, and possibly the working tree.
With a prefix argument reset the working tree otherwise don't.

(git reset --mixed|--hard COMMIT)

(fn COMMIT &optional HARD)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-reset" '("magit-reset-"))) (autoload 'magit-sequencer-continue "magit-sequence" "Resume the current cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-skip "magit-sequence" "Skip the stopped at commit during a cherry-pick or revert sequence." t nil) (autoload 'magit-sequencer-abort "magit-sequence" "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-cherry-pick "magit-sequence" nil t) (autoload 'magit-cherry-copy "magit-sequence" "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-apply "magit-sequence" "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting.

(fn COMMITS &optional ARGS)" t nil) (autoload 'magit-cherry-harvest "magit-sequence" "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-donate "magit-sequence" "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.  `HEAD' is allowed to be detached initially.

(fn COMMITS BRANCH &optional ARGS)" t nil) (autoload 'magit-cherry-spinout "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-cherry-spinoff "magit-sequence" "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually.

(fn COMMITS BRANCH START-POINT &optional ARGS)" t nil) (autoload 'magit-revert "magit-sequence" nil t) (autoload 'magit-revert-and-commit "magit-sequence" "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-revert-no-commit "magit-sequence" "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting.

(fn COMMIT &optional ARGS)" t nil) (autoload 'magit-am "magit-sequence" nil t) (autoload 'magit-am-apply-patches "magit-sequence" "Apply the patches FILES.

(fn &optional FILES ARGS)" t nil) (autoload 'magit-am-apply-maildir "magit-sequence" "Apply the patches from MAILDIR.

(fn &optional MAILDIR ARGS)" t nil) (autoload 'magit-am-continue "magit-sequence" "Resume the current patch applying sequence." t nil) (autoload 'magit-am-skip "magit-sequence" "Skip the stopped at patch during a patch applying sequence." t nil) (autoload 'magit-am-abort "magit-sequence" "Abort the current patch applying sequence.
This discards all changes made since the sequence started." t nil) (autoload 'magit-rebase "magit-sequence" nil t) (autoload 'magit-rebase-onto-pushremote "magit-sequence" nil t) (autoload 'magit-rebase-onto-upstream "magit-sequence" nil t) (autoload 'magit-rebase-branch "magit-sequence" "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased.

(fn TARGET ARGS)" t nil) (autoload 'magit-rebase-subset "magit-sequence" "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits.

(fn NEWBASE START ARGS)" t nil) (autoload 'magit-rebase-interactive "magit-sequence" "Start an interactive rebase sequence.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-autosquash "magit-sequence" "Combine squash and fixup commits with their intended targets.

(fn ARGS)" t nil) (autoload 'magit-rebase-edit-commit "magit-sequence" "Edit a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-reword-commit "magit-sequence" "Reword a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-remove-commit "magit-sequence" "Remove a single older commit using rebase.

(fn COMMIT ARGS)" t nil) (autoload 'magit-rebase-continue "magit-sequence" "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is.

(fn &optional NOEDIT)" t nil) (autoload 'magit-rebase-skip "magit-sequence" "Skip the current commit and restart the current rebase operation." t nil) (autoload 'magit-rebase-edit "magit-sequence" "Edit the todo list of the current rebase operation." t nil) (autoload 'magit-rebase-abort "magit-sequence" "Abort the current rebase operation, restoring the original branch." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-sequence" '("magit-"))) (autoload 'magit-stash "magit-stash" nil t) (autoload 'magit-stash-both "magit-stash" "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-index "magit-stash" "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect.

(fn MESSAGE)" t nil) (autoload 'magit-stash-worktree "magit-stash" "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-keep-index "magit-stash" "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn MESSAGE &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-both "magit-stash" "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-snapshot-index "magit-stash" "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed." t nil) (autoload 'magit-snapshot-worktree "magit-stash" "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'.

(fn &optional INCLUDE-UNTRACKED)" t nil) (autoload 'magit-stash-apply "magit-stash" "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index.

(fn STASH)" t nil) (autoload 'magit-stash-pop "magit-stash" "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash.

(fn STASH)" t nil) (autoload 'magit-stash-drop "magit-stash" "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.

(fn STASH)" t nil) (autoload 'magit-stash-clear "magit-stash" "Remove all stashes saved in REF's reflog by deleting REF.

(fn REF)" t nil) (autoload 'magit-stash-branch "magit-stash" "Create and checkout a new BRANCH from STASH.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-branch-here "magit-stash" "Create and checkout a new BRANCH and apply STASH.
The branch is created using `magit-branch-and-checkout', using the
current branch or `HEAD' as the start-point.

(fn STASH BRANCH)" t nil) (autoload 'magit-stash-format-patch "magit-stash" "Create a patch from STASH

(fn STASH)" t nil) (autoload 'magit-stash-list "magit-stash" "List all stashes in a buffer." t nil) (autoload 'magit-stash-show "magit-stash" "Show all diffs of a stash in a buffer.

(fn STASH &optional ARGS FILES)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-stash" '("magit-"))) (autoload 'magit-init "magit-status" "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally.

(fn DIRECTORY)" t nil) (autoload 'magit-status "magit-status" "Show the status of the current Git repository in a buffer.

If the current directory isn't located within a Git repository,
then prompt for an existing repository or an arbitrary directory,
depending on option `magit-repository-directories', and show the
status of the selected repository instead.

* If that option specifies any existing repositories, then offer
  those for completion and show the status buffer for the
  selected one.

* Otherwise read an arbitrary directory using regular file-name
  completion.  If the selected directory is the top-level of an
  existing working tree, then show the status buffer for that.

* Otherwise offer to initialize the selected directory as a new
  repository.  After creating the repository show its status
  buffer.

These fallback behaviors can also be forced using one or more
prefix arguments:

* With two prefix arguments (or more precisely a numeric prefix
  value of 16 or greater) read an arbitrary directory and act on
  it as described above.  The same could be accomplished using
  the command `magit-init'.

* With a single prefix argument read an existing repository, or
  if none can be found based on `magit-repository-directories',
  then fall back to the same behavior as with two prefix
  arguments.

(fn &optional DIRECTORY CACHE)" t nil) (defalias 'magit 'magit-status "An alias for `magit-status' for better discoverability.

Instead of invoking this alias for `magit-status' using
\"M-x magit RET\", you should bind a key to `magit-status'
and read the info node `(magit)Getting Started', which
also contains other useful hints.") (autoload 'magit-status-here "magit-status" "Like `magit-status' but with non-nil `magit-status-goto-file-position'." t nil) (autoload 'magit-status-quick "magit-status" "Show the status of the current Git repository, maybe without refreshing.

If the status buffer of the current Git repository exists but
isn't being displayed in the selected frame, then display it
without refreshing it.

If the status buffer is being displayed in the selected frame,
then also refresh it.

Prefix arguments have the same meaning as for `magit-status',
and additionally cause the buffer to be refresh.

To use this function instead of `magit-status', add this to your
init file: (global-set-key (kbd \"C-x g\") 'magit-status-quick)." t nil) (autoload 'magit-status-setup-buffer "magit-status" "

(fn &optional DIRECTORY)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-status" '("magit-"))) (autoload 'magit-submodule "magit-submodule" nil t) (autoload 'magit-submodule-add "magit-submodule" nil t) (autoload 'magit-submodule-read-name-for-path "magit-submodule" "

(fn PATH &optional PREFER-SHORT)" nil nil) (autoload 'magit-submodule-register "magit-submodule" nil t) (autoload 'magit-submodule-populate "magit-submodule" nil t) (autoload 'magit-submodule-update "magit-submodule" nil t) (autoload 'magit-submodule-synchronize "magit-submodule" nil t) (autoload 'magit-submodule-unpopulate "magit-submodule" nil t) (autoload 'magit-submodule-remove "magit-submodule" "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it.

(fn MODULES ARGS TRASH-GITDIRS)" t nil) (autoload 'magit-insert-modules "magit-submodule" "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section." nil nil) (autoload 'magit-insert-modules-overview "magit-submodule" "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash." nil nil) (autoload 'magit-insert-modules-unpulled-from-upstream "magit-submodule" "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpulled-from-pushremote "magit-submodule" "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-upstream "magit-submodule" "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-insert-modules-unpushed-to-pushremote "magit-submodule" "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits." nil nil) (autoload 'magit-list-submodules "magit-submodule" "Display a list of the current repository's submodules." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-submodule" '("magit-"))) (autoload 'magit-subtree "magit-subtree" nil t) (autoload 'magit-subtree-import "magit-subtree" nil t) (autoload 'magit-subtree-export "magit-subtree" nil t) (autoload 'magit-subtree-add "magit-subtree" "Add REF from REPOSITORY as a new subtree at PREFIX.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-add-commit "magit-subtree" "Add COMMIT as a new subtree at PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-merge "magit-subtree" "Merge COMMIT into the PREFIX subtree.

(fn PREFIX COMMIT ARGS)" t nil) (autoload 'magit-subtree-pull "magit-subtree" "Pull REF from REPOSITORY into the PREFIX subtree.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-push "magit-subtree" "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY.

(fn PREFIX REPOSITORY REF ARGS)" t nil) (autoload 'magit-subtree-split "magit-subtree" "Extract the history of the subtree PREFIX.

(fn PREFIX COMMIT ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-subtree" '("magit-"))) (autoload 'magit-tag "magit" nil t) (autoload 'magit-tag-create "magit-tag" "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.

(git tag [--annotate] NAME REV)

(fn NAME REV &optional ARGS)" t nil) (autoload 'magit-tag-delete "magit-tag" "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.

(git tag -d TAGS)

(fn TAGS)" t nil) (autoload 'magit-tag-prune "magit-tag" "Offer to delete tags missing locally from REMOTE, and vice versa.

(fn TAGS REMOTE-TAGS REMOTE)" t nil) (autoload 'magit-tag-release "magit-tag" "Create a release tag.

Assume that release tags match `magit-release-tag-regexp'.

First prompt for the name of the new tag using the highest
existing tag as initial input and leaving it to the user to
increment the desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\".

(fn TAG MSG &optional ARGS)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-tag" '("magit-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-transient" '("magit-"))) (autoload 'magit-emacs-Q-command "magit-utils" "Show a shell command that runs an uncustomized Emacs with only Magit loaded.
See info node `(magit)Debugging Tools' for more information." t nil) (autoload 'Info-follow-nearest-node--magit-gitman "magit-utils" "

(fn FN &optional FORK)" nil nil) (advice-add 'Info-follow-nearest-node :around 'Info-follow-nearest-node--magit-gitman) (autoload 'org-man-export--magit-gitman "magit-utils" "

(fn FN LINK DESCRIPTION FORMAT)" nil nil) (advice-add 'org-man-export :around 'org-man-export--magit-gitman) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-utils" '("magit-"))) (defvar magit-wip-mode nil "Non-nil if Magit-Wip mode is enabled.
See the `magit-wip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.") (custom-autoload 'magit-wip-mode "magit-wip" nil) (autoload 'magit-wip-mode "magit-wip" "Save uncommitted changes to work-in-progress refs.

If called interactively, enable Magit-Wip mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Whenever appropriate (i.e. when dataloss would be a possibility
otherwise) this mode causes uncommitted changes to be committed
to dedicated work-in-progress refs.

For historic reasons this mode is implemented on top of four
other `magit-wip-*' modes, which can also be used individually,
if you want finer control over when the wip refs are updated;
but that is discouraged.

(fn &optional ARG)" t nil) (put 'magit-wip-after-save-mode 'globalized-minor-mode t) (defvar magit-wip-after-save-mode nil "Non-nil if Magit-Wip-After-Save mode is enabled.
See the `magit-wip-after-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-after-save-mode'.") (custom-autoload 'magit-wip-after-save-mode "magit-wip" nil) (autoload 'magit-wip-after-save-mode "magit-wip" "Toggle Magit-Wip-After-Save-Local mode in all buffers.
With prefix ARG, enable Magit-Wip-After-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-After-Save-Local mode is enabled in all buffers where
`magit-wip-after-save-local-mode-turn-on' would do it.
See `magit-wip-after-save-local-mode' for more information on Magit-Wip-After-Save-Local mode.

(fn &optional ARG)" t nil) (defvar magit-wip-after-apply-mode nil "Non-nil if Magit-Wip-After-Apply mode is enabled.
See the `magit-wip-after-apply-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-after-apply-mode "magit-wip" nil) (autoload 'magit-wip-after-apply-mode "magit-wip" "Commit to work-in-progress refs.

If called interactively, enable Magit-Wip-After-Apply mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index.

(fn &optional ARG)" t nil) (defvar magit-wip-before-change-mode nil "Non-nil if Magit-Wip-Before-Change mode is enabled.
See the `magit-wip-before-change-mode' command
for a description of this minor mode.") (custom-autoload 'magit-wip-before-change-mode "magit-wip" nil) (autoload 'magit-wip-before-change-mode "magit-wip" "Commit to work-in-progress refs before certain destructive changes.

If called interactively, enable Magit-Wip-Before-Change mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed.

(fn &optional ARG)" t nil) (autoload 'magit-wip-commit-initial-backup "magit-wip" "Before saving, commit current file to a worktree wip ref.

The user has to add this function to `before-save-hook'.

Commit the current state of the visited file before saving the
current buffer to that file.  This backs up the same version of
the file as `backup-buffer' would, but stores the backup in the
worktree wip ref, which is also used by the various Magit Wip
modes, instead of in a backup file as `backup-buffer' would.

This function ignores the variables that affect `backup-buffer'
and can be used along-side that function, which is recommended
because this function only backs up files that are tracked in
a Git repository." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-wip" '("magit-"))) (autoload 'magit-worktree "magit-worktree" nil t) (autoload 'magit-worktree-checkout "magit-worktree" "Checkout BRANCH in a new worktree at PATH.

(fn PATH BRANCH)" t nil) (autoload 'magit-worktree-branch "magit-worktree" "Create a new BRANCH and check it out in a new worktree at PATH.

(fn PATH BRANCH START-POINT &optional FORCE)" t nil) (autoload 'magit-worktree-move "magit-worktree" "Move WORKTREE to PATH.

(fn WORKTREE PATH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-worktree" '("magit-"))) (provide 'magit-autoloads)) "beacon" ((beacon-autoloads beacon) (autoload 'beacon-blink "beacon" "Blink the beacon at the position of the cursor.
Unlike `beacon-blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from lisp code." t nil) (defvar beacon-mode nil "Non-nil if Beacon mode is enabled.
See the `beacon-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `beacon-mode'.") (custom-autoload 'beacon-mode "beacon" nil) (autoload 'beacon-mode "beacon" "Toggle Beacon mode on or off.

If called interactively, enable Beacon mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{beacon-mode-map}

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "beacon" '("beacon-"))) (provide 'beacon-autoloads)) "smartparens" ((smartparens-scala smartparens-ruby smartparens-racket smartparens-text smartparens-ess smartparens-rst smartparens-javascript smartparens-html smartparens-rust smartparens-org smartparens-markdown sp-sublimetext-like smartparens-autoloads smartparens-c smartparens-python smartparens-crystal smartparens-lua smartparens-clojure smartparens-ml smartparens-latex smartparens-haskell smartparens-elixir smartparens-pkg smartparens smartparens-config) (autoload 'sp-cheat-sheet "smartparens" "Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument ARG, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation.

(fn &optional ARG)" t nil) (defvar smartparens-mode-map (make-sparse-keymap) "Keymap used for `smartparens-mode'.") (autoload 'sp-use-paredit-bindings "smartparens" "Initiate `smartparens-mode-map' with `sp-paredit-bindings'." t nil) (autoload 'sp-use-smartparens-bindings "smartparens" "Initiate `smartparens-mode-map' with `sp-smartparens-bindings'." t nil) (autoload 'smartparens-mode "smartparens" "Toggle smartparens mode.

If called interactively, enable Smartparens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

You can enable pre-set bindings by customizing
`sp-base-key-bindings' variable.  The current content of
`smartparens-mode-map' is:

 \\{smartparens-mode-map}

(fn &optional ARG)" t nil) (autoload 'smartparens-strict-mode "smartparens" "Toggle the strict smartparens mode.

If called interactively, enable Smartparens-Strict mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

When strict mode is active, `delete-char', `kill-word' and their
backward variants will skip over the pair delimiters in order to
keep the structure always valid (the same way as `paredit-mode'
does).  This is accomplished by remapping them to
`sp-delete-char' and `sp-kill-word'.  There is also function
`sp-kill-symbol' that deletes symbols instead of words, otherwise
working exactly the same (it is not bound to any key by default).

When strict mode is active, this is indicated with \"/s\"
after the smartparens indicator in the mode list.

(fn &optional ARG)" t nil) (put 'smartparens-global-strict-mode 'globalized-minor-mode t) (defvar smartparens-global-strict-mode nil "Non-nil if Smartparens-Global-Strict mode is enabled.
See the `smartparens-global-strict-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-strict-mode'.") (custom-autoload 'smartparens-global-strict-mode "smartparens" nil) (autoload 'smartparens-global-strict-mode "smartparens" "Toggle Smartparens-Strict mode in all buffers.
With prefix ARG, enable Smartparens-Global-Strict mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens-Strict mode is enabled in all buffers where
`turn-on-smartparens-strict-mode' would do it.
See `smartparens-strict-mode' for more information on Smartparens-Strict mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-smartparens-strict-mode "smartparens" "Turn on `smartparens-strict-mode'." t nil) (autoload 'turn-off-smartparens-strict-mode "smartparens" "Turn off `smartparens-strict-mode'." t nil) (put 'smartparens-global-mode 'globalized-minor-mode t) (defvar smartparens-global-mode nil "Non-nil if Smartparens-Global mode is enabled.
See the `smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.") (custom-autoload 'smartparens-global-mode "smartparens" nil) (autoload 'smartparens-global-mode "smartparens" "Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.
See `smartparens-mode' for more information on Smartparens mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-smartparens-mode "smartparens" "Turn on `smartparens-mode'.

This function is used to turn on `smartparens-global-mode'.

By default `smartparens-global-mode' ignores buffers with
`mode-class' set to special, but only if they are also not comint
buffers.

Additionally, buffers on `sp-ignore-modes-list' are ignored.

You can still turn on smartparens in these mode manually (or
in mode's startup-hook etc.) by calling `smartparens-mode'." t nil) (autoload 'turn-off-smartparens-mode "smartparens" "Turn off `smartparens-mode'." t nil) (autoload 'show-smartparens-mode "smartparens" "Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs.

If called interactively, enable Show-Smartparens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'show-smartparens-global-mode 'globalized-minor-mode t) (defvar show-smartparens-global-mode nil "Non-nil if Show-Smartparens-Global mode is enabled.
See the `show-smartparens-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `show-smartparens-global-mode'.") (custom-autoload 'show-smartparens-global-mode "smartparens" nil) (autoload 'show-smartparens-global-mode "smartparens" "Toggle Show-Smartparens mode in all buffers.
With prefix ARG, enable Show-Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Show-Smartparens mode is enabled in all buffers where
`turn-on-show-smartparens-mode' would do it.
See `show-smartparens-mode' for more information on Show-Smartparens mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-show-smartparens-mode "smartparens" "Turn on `show-smartparens-mode'." t nil) (autoload 'turn-off-show-smartparens-mode "smartparens" "Turn off `show-smartparens-mode'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens" '("smartparens-" "sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-clojure" '("sp-clojure-prefix"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-config" '("sp-lisp-invalid-hyperlink-p"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-crystal" '("sp-crystal-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-elixir" '("sp-elixir-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ess" '("sp-ess-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-haskell" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-html" '("sp-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-latex" '("sp-latex-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-lua" '("sp-lua-post-keyword-insert"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-markdown" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-org" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-python" '("sp-python-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rst" '("sp-rst-point-after-backtick"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-ruby" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-rust" '("sp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-scala" '("sp-scala-wrap-with-indented-newlines"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smartparens-text" '("sp-text-mode-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sp-sublimetext-like" '("sp-point-not-before-word"))) (provide 'smartparens-autoloads)) "auctex" ((tex-buf tex-style bib-cite multi-prompt tex-site \.dir-locals tex-jp plain-tex font-latex tex-info lpath preview texmathp latex auctex tex-wizard toolbar-x latex-flymake tex-font tex tex-mik context-nl context tex-ispell auctex-autoloads tex-bar tex-fold context-en) (autoload 'bib-cite-minor-mode "bib-cite" "Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs `bib-find', and [mouse-3] runs `bib-display'.

(fn ARG)" t nil) (autoload 'turn-on-bib-cite "bib-cite" "Unconditionally turn on Bib Cite mode." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bib-cite" '("LaTeX-" "bib-" "create-alist-from-list" "member-cis" "psg-" "search-directory-tree"))) (defalias 'ConTeXt-mode #'context-mode) (autoload 'context-mode "context" "Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of ConTeXt-mode-hook." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "context" '("ConTeXt-" "TeX-ConTeXt-sentinel" "context-guess-current-interface"))) (autoload 'context-en-mode "context-en" "Major mode for editing files for ConTeXt using its english interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "context-en" '("ConTeXt-"))) (autoload 'context-nl-mode "context-nl" "Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "context-nl" '("ConTeXt-"))) (autoload 'font-latex-setup "font-latex" "Setup this buffer for LaTeX font-lock.  Usually called from a hook." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "font-latex" '("font-latex-"))) (autoload 'BibTeX-auto-store "latex" "This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file." nil nil) (add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode) t) (add-to-list 'auto-mode-alist '("\\.hva\\'" . latex-mode)) (autoload 'TeX-latex-mode "latex" "Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'." t nil) (add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode)) (autoload 'docTeX-mode "latex" "Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'.

(fn)" t nil) (defalias 'TeX-doctex-mode #'docTeX-mode) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "latex" '("Bib" "LaTeX-" "TeX-" "docTeX-" "latex-math-mode"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "latex-flymake" '("LaTeX-"))) (autoload 'multi-prompt "multi-prompt" "Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that.

(fn SEPARATOR UNIQUE PROMPT TABLE &optional MP-PREDICATE REQUIRE-MATCH INITIAL HISTORY)" nil nil) (autoload 'multi-prompt-key-value "multi-prompt" "Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer.

(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-prompt" '("multi-prompt-"))) (autoload 'TeX-plain-tex-mode "plain-tex" "Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `plain-TeX-mode-hook'." t nil) (autoload 'ams-tex-mode "plain-tex" "Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering `ams-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "plain-tex" '("AmS" "plain-TeX-"))) (autoload 'preview-install-styles "preview" "Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path.

(fn DIR &optional FORCE-OVERWRITE FORCE-SAVE)" t nil) (autoload 'LaTeX-preview-setup "preview" "Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality." nil nil) (autoload 'preview-report-bug "preview" "Report a bug in the preview-latex package." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "preview" '("TeX-" "desktop-buffer-preview" "preview-"))) (autoload 'TeX-tex-mode "tex" "Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'" t nil) (autoload 'TeX-auto-generate "tex" "Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory.

(fn TEX AUTO)" t nil) (autoload 'TeX-auto-generate-global "tex" "Create global auto directory for global TeX macro definitions." t nil) (autoload 'TeX-submit-bug-report "tex" "Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex" '("Bib" "ConTeXt-" "LaTeX-" "TeX-" "VirTeX-common-initialization" "docTeX-default-extension" "plain-TeX-auto-regexp-list" "tex-"))) (autoload 'TeX-install-toolbar "tex-bar" "Install toolbar buttons for TeX mode." t nil) (autoload 'LaTeX-install-toolbar "tex-bar" "Install toolbar buttons for LaTeX mode." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-bar" '("TeX-bar-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-buf" '("LaTeX-" "TeX-"))) (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off.

(fn &optional ARG)" t nil) (defalias 'tex-fold-mode #'TeX-fold-mode) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-fold" '("TeX-fold-"))) (autoload 'tex-font-setup "tex-font" "Setup font lock support for TeX." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-font" '("tex-"))) (defalias 'Texinfo-mode #'texinfo-mode) (autoload 'TeX-texinfo-mode "tex-info" "Major mode in AUCTeX for editing Texinfo files.

Special commands:
\\{Texinfo-mode-map}

Entering Texinfo mode calls the value of `text-mode-hook'  and then the
value of `Texinfo-mode-hook'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-info" '("Texinfo-" "texinfo-environment-regexp"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-ispell" '("TeX-ispell-"))) (autoload 'japanese-plain-tex-mode "tex-jp" "Major mode in AUCTeX for editing Japanese plain TeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-plain-tex-mode'." t nil) (autoload 'japanese-latex-mode "tex-jp" "Major mode in AUCTeX for editing Japanese LaTeX files.
Set `japanese-TeX-mode' to t, and enter `TeX-latex-mode'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-jp" '("TeX-" "japanese-"))) (require 'tex-site) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-site" '("AUCTeX-" "TeX-" "preview-TeX-style-dir"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-style" '("LaTeX-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tex-wizard" '("TeX-wizard"))) (autoload 'texmathp "texmathp" "Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked." t nil) (autoload 'texmathp-match-switch "texmathp" "Search backward for any of the math switches.
Limit searched to BOUND.

(fn BOUND)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "texmathp" '("texmathp-"))) (autoload 'toolbarx-install-toolbar "toolbar-x") (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toolbar-x" '("toolbarx-"))) (provide 'auctex-autoloads)) "avy" ((avy avy-autoloads) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)" nil nil) (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t nil) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t nil) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t nil) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t nil) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t nil) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t nil) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t nil) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t nil) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t nil) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t nil) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t nil) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t nil) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t nil) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t nil) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t nil) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t nil) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don\342\200\231t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t nil) (autoload 'avy-setup-default "avy" "Setup the default shortcuts." nil nil) (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t nil) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "avy" '("avy-"))) (provide 'avy-autoloads)) "ace-window" ((ace-window-autoloads ace-window) (autoload 'ace-select-window "ace-window" "Ace select window." t nil) (autoload 'ace-delete-window "ace-window" "Ace delete window." t nil) (autoload 'ace-swap-window "ace-window" "Ace swap window." t nil) (autoload 'ace-delete-other-windows "ace-window" "Ace delete other windows." t nil) (autoload 'ace-display-buffer "ace-window" "Make `display-buffer' and `pop-to-buffer' select using `ace-window'.
See sample config for `display-buffer-base-action' and `display-buffer-alist':
https://github.com/abo-abo/ace-window/wiki/display-buffer.

(fn BUFFER ALIST)" nil nil) (autoload 'ace-window "ace-window" "Select a window.
Perform an action based on ARG described below.

By default, behaves like extended `other-window'.
See `aw-scope' which extends it to work with frames.

Prefixed with one \\[universal-argument], does a swap between the
selected window and the current window, so that the selected
buffer moves to current window (and current buffer moves to
selected window).

Prefixed with two \\[universal-argument]'s, deletes the selected
window.

(fn ARG)" t nil) (defvar ace-window-display-mode nil "Non-nil if Ace-Window-Display mode is enabled.
See the `ace-window-display-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ace-window-display-mode'.") (custom-autoload 'ace-window-display-mode "ace-window" nil) (autoload 'ace-window-display-mode "ace-window" "Minor mode for showing the ace window key in the mode line.

If called interactively, enable Ace-Window-Display mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ace-window" '("ace-window-mode" "aw-"))) (provide 'ace-window-autoloads)) "multiple-cursors" ((mc-separate-operations mc-cycle-cursors mc-mark-more multiple-cursors mc-hide-unmatched-lines-mode mc-edit-lines multiple-cursors-core rectangular-region-mode multiple-cursors-pkg mc-mark-pop multiple-cursors-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-cycle-cursors" '("mc/"))) (autoload 'mc/edit-lines "mc-edit-lines" "Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

What is done with lines which are not long enough is governed by
`mc/edit-lines-empty-lines'.  The prefix argument ARG can be used
to override this.  If ARG is a symbol (when called from Lisp),
that symbol is used instead of `mc/edit-lines-empty-lines'.
Otherwise, if ARG negative, short lines will be ignored.  Any
other non-nil value will cause short lines to be padded.

(fn &optional ARG)" t nil) (autoload 'mc/edit-ends-of-lines "mc-edit-lines" "Add one cursor to the end of each line in the active region." t nil) (autoload 'mc/edit-beginnings-of-lines "mc-edit-lines" "Add one cursor to the beginning of each line in the active region." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-edit-lines" '("mc/edit-lines-empty-lines"))) (autoload 'mc-hide-unmatched-lines-mode "mc-hide-unmatched-lines-mode" "Minor mode when enabled hides all lines where no cursors (and
also hum/lines-to-expand below and above) To make use of this
mode press \"C-'\" while multiple-cursor-mode is active. You can
still edit lines while you are in mc-hide-unmatched-lines
mode. To leave this mode press <return> or \"C-g\"

If called interactively, enable Mc-Hide-Unmatched-Lines mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-hide-unmatched-lines-mode" '("hum/"))) (autoload 'mc/mark-next-like-this "mc-mark-more" "Find and mark the next part of the buffer matching the currently active region
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-like-this-word "mc-mark-more" "Find and mark the next part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the next match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-word-like-this "mc-mark-more" "Find and mark the next word of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-symbol-like-this "mc-mark-more" "Find and mark the next symbol of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active add a cursor on the next line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-previous-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-previous-like-this-word "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
If no region is active, mark the word at the point and find the previous match
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous.

(fn ARG)" t nil) (autoload 'mc/mark-previous-word-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
The matching region must be a whole word to be a match
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-previous-symbol-like-this "mc-mark-more" "Find and mark the previous part of the buffer matching the currently active region
The matching region must be a whole symbol to be a match
If no region is active add a cursor on the previous line
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next.

(fn ARG)" t nil) (autoload 'mc/mark-next-lines "mc-mark-more" "

(fn ARG)" t nil) (autoload 'mc/mark-previous-lines "mc-mark-more" "

(fn ARG)" t nil) (autoload 'mc/unmark-next-like-this "mc-mark-more" "Deselect next part of the buffer matching the currently active region." t nil) (autoload 'mc/unmark-previous-like-this "mc-mark-more" "Deselect prev part of the buffer matching the currently active region." t nil) (autoload 'mc/skip-to-next-like-this "mc-mark-more" "Skip the current one and select the next part of the buffer matching the currently active region." t nil) (autoload 'mc/skip-to-previous-like-this "mc-mark-more" "Skip the current one and select the prev part of the buffer matching the currently active region." t nil) (autoload 'mc/mark-all-like-this "mc-mark-more" "Find and mark all the parts of the buffer matching the currently active region" t nil) (autoload 'mc/mark-all-words-like-this "mc-mark-more" nil t nil) (autoload 'mc/mark-all-symbols-like-this "mc-mark-more" nil t nil) (autoload 'mc/mark-all-in-region "mc-mark-more" "Find and mark all the parts in the region matching the given search

(fn BEG END &optional SEARCH)" t nil) (autoload 'mc/mark-all-in-region-regexp "mc-mark-more" "Find and mark all the parts in the region matching the given regexp.

(fn BEG END)" t nil) (autoload 'mc/mark-more-like-this-extended "mc-mark-more" "Like mark-more-like-this, but then lets you adjust with arrows key.
The adjustments work like this:

   <up>    Mark previous like this and set direction to 'up
   <down>  Mark next like this and set direction to 'down

If direction is 'up:

   <left>  Skip past the cursor furthest up
   <right> Remove the cursor furthest up

If direction is 'down:

   <left>  Remove the cursor furthest down
   <right> Skip past the cursor furthest down

The bindings for these commands can be changed. See `mc/mark-more-like-this-extended-keymap'." t nil) (autoload 'mc/mark-all-like-this-dwim "mc-mark-more" "Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'

(fn ARG)" t nil) (autoload 'mc/mark-all-dwim "mc-mark-more" "Tries even harder to guess what you want to mark all of.

If the region is active and spans multiple lines, it will behave
as if `mc/mark-all-in-region'. With the prefix ARG, it will call
`mc/edit-lines' instead.

If the region is inactive or on a single line, it will behave like
`mc/mark-all-like-this-dwim'.

(fn ARG)" t nil) (autoload 'mc/mark-all-like-this-in-defun "mc-mark-more" "Mark all like this in defun." t nil) (autoload 'mc/mark-all-words-like-this-in-defun "mc-mark-more" "Mark all words like this in defun." t nil) (autoload 'mc/mark-all-symbols-like-this-in-defun "mc-mark-more" "Mark all symbols like this in defun." t nil) (autoload 'mc/toggle-cursor-on-click "mc-mark-more" "Add a cursor where you click, or remove a fake cursor that is
already there.

(fn EVENT)" t nil) (defalias 'mc/add-cursor-on-click 'mc/toggle-cursor-on-click) (autoload 'mc/mark-sgml-tag-pair "mc-mark-more" "Mark the tag we're in and its pair for renaming." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-mark-more" '("mc--" "mc/"))) (autoload 'mc/mark-pop "mc-mark-pop" "Add a cursor at the current point, pop off mark ring and jump
to the popped mark." t nil) (autoload 'mc/insert-numbers "mc-separate-operations" "Insert increasing numbers for each cursor, starting at
`mc/insert-numbers-default' or ARG.

(fn ARG)" t nil) (autoload 'mc/insert-letters "mc-separate-operations" "Insert increasing letters for each cursor, starting at 0 or ARG.
     Where letter[0]=a letter[2]=c letter[26]=aa

(fn ARG)" t nil) (autoload 'mc/reverse-regions "mc-separate-operations" nil t nil) (autoload 'mc/sort-regions "mc-separate-operations" nil t nil) (autoload 'mc/vertical-align "mc-separate-operations" "Aligns all cursors vertically with a given CHARACTER to the one with the
highest column number (the rightest).
Might not behave as intended if more than one cursors are on the same line.

(fn CHARACTER)" t nil) (autoload 'mc/vertical-align-with-space "mc-separate-operations" "Aligns all cursors with whitespace like `mc/vertical-align' does" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mc-separate-operations" '("mc--" "mc/insert-numbers-default"))) (autoload 'multiple-cursors-mode "multiple-cursors-core" "Mode while multiple cursors are active.

If called interactively, enable Multiple-Cursors mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multiple-cursors-core" '("activate-cursor-for-undo" "deactivate-cursor-after-undo" "multiple-cursors-mode" "unsupported-cmd"))) (autoload 'set-rectangular-region-anchor "rectangular-region-mode" "Anchors the rectangular region at point.

Think of this one as `set-mark' except you're marking a rectangular region. It is
an exceedingly quick way of adding multiple cursors to multiple lines." t nil) (autoload 'rectangular-region-mode "rectangular-region-mode" "A mode for creating a rectangular region to edit

If called interactively, enable Rectangular-Region mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rectangular-region-mode" '("rectangular-region-mode" "rrm/"))) (provide 'multiple-cursors-autoloads)) "epl" ((epl epl-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "epl" '("epl-"))) (provide 'epl-autoloads)) "pkg-info" ((pkg-info-autoloads pkg-info) (autoload 'pkg-info-library-original-version "pkg-info" "Get the original version in the header of LIBRARY.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no X-Original-Version
header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-library-version "pkg-info" "Get the version in the header of LIBRARY.

LIBRARY is either a symbol denoting a named feature, or a library
name as string.

If SHOW is non-nil, show the version in the minibuffer.

Return the version from the header of LIBRARY as list.  Signal an
error if the LIBRARY was not found or had no proper header.

See Info node `(elisp)Library Headers' for more information
about library headers.

(fn LIBRARY &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-original-version "pkg-info" "Get the original version of the library defining FUNCTION.

The original version is stored in the X-Original-Version header.
This header is added by the MELPA package archive to preserve
upstream version numbers.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-defining-library-version "pkg-info" "Get the version of the library defining FUNCTION.

If SHOW is non-nil, show the version in mini-buffer.

This function is mainly intended to find the version of a major
or minor mode, i.e.

   (pkg-info-defining-library-version 'flycheck-mode)

Return the version of the library defining FUNCTION.  Signal an
error if FUNCTION is not a valid function, if its defining
library was not found, or if the library had no proper version
header.

(fn FUNCTION &optional SHOW)" t nil) (autoload 'pkg-info-package-version "pkg-info" "Get the version of an installed PACKAGE.

If SHOW is non-nil, show the version in the minibuffer.

Return the version as list, or nil if PACKAGE is not installed.

(fn PACKAGE &optional SHOW)" t nil) (autoload 'pkg-info-version-info "pkg-info" "Obtain complete version info for LIBRARY and PACKAGE.

LIBRARY is a symbol denoting a named feature, or a library name
as string.  PACKAGE is a symbol denoting an ELPA package.  If
omitted or nil, default to LIBRARY.

If SHOW is non-nil, show the version in the minibuffer.

When called interactively, prompt for LIBRARY.  When called
interactively with prefix argument, prompt for PACKAGE as well.

Return a string with complete version information for LIBRARY.
This version information contains the version from the headers of
LIBRARY, and the version of the installed PACKAGE, the LIBRARY is
part of.  If PACKAGE is not installed, or if the PACKAGE version
is the same as the LIBRARY version, do not include a package
version.

(fn LIBRARY &optional PACKAGE SHOW)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pkg-info" '("pkg-info-"))) (provide 'pkg-info-autoloads)) "let-alist" ((let-alist-autoloads let-alist) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one.  You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "let-alist" '("let-alist--"))) (provide 'let-alist-autoloads)) "flycheck" ((flycheck-ert flycheck flycheck-buttercup flycheck-autoloads) (autoload 'flycheck-manual "flycheck" "Open the Flycheck manual." t nil) (autoload 'flycheck-mode "flycheck" "Flycheck is a minor mode for on-the-fly syntax checking.

In `flycheck-mode' the buffer is automatically syntax-checked
using the first suitable syntax checker from `flycheck-checkers'.
Use `flycheck-select-checker' to select a checker for the current
buffer manually.

If you run into issues, use `\\[flycheck-verify-setup]' to get help.

Flycheck supports many languages out of the box, and many
additional ones are available on MELPA.  Adding new ones is very
easy.  Complete documentation is available online at URL
`https://www.flycheck.org/en/latest/'.  Please report issues and
request features at URL `https://github.com/flycheck/flycheck'.

Flycheck displays its status in the mode line.  In the default
configuration, it looks like this:

`FlyC'     This buffer has not been checked yet.
`FlyC-'    Flycheck doesn't have a checker for this buffer.
`FlyC*'    Flycheck is running.  Expect results soon!
`FlyC:3|2' This buffer contains three warnings and two errors.
           Use `\\[flycheck-list-errors]' to see the list.

You may also see the following icons:
`FlyC!'    The checker crashed.
`FlyC.'    The last syntax check was manually interrupted.
`FlyC?'    The checker did something unexpected, like exiting with 1
           but returning no errors.

The following keybindings are available in `flycheck-mode':

\\{flycheck-mode-map}
(you can change the prefix by customizing
`flycheck-keymap-prefix')

If called interactively, enable Flycheck mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is \342\200\230toggle\342\200\231; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-flycheck-mode 'globalized-minor-mode t) (defvar global-flycheck-mode nil "Non-nil if Global Flycheck mode is enabled.
See the `global-flycheck-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-flycheck-mode'.") (custom-autoload 'global-flycheck-mode "flycheck" nil) (autoload 'global-flycheck-mode "flycheck" "Toggle Flycheck mode in all buffers.
With prefix ARG, enable Global Flycheck mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Flycheck mode is enabled in all buffers where
`flycheck-mode-on-safe' would do it.
See `flycheck-mode' for more information on Flycheck mode.

(fn &optional ARG)" t nil) (autoload 'flycheck-define-error-level "flycheck" "Define a new error LEVEL with PROPERTIES.

The following PROPERTIES constitute an error level:

`:severity SEVERITY'
     A number denoting the severity of this level.  The higher
     the number, the more severe is this level compared to other
     levels.  Defaults to 0; info is -10, warning is 10, and
     error is 100.

     The severity is used by `flycheck-error-level-<' to
     determine the ordering of errors according to their levels.

`:compilation-level LEVEL'

     A number indicating the broad class of messages that errors
     at this level belong to: one of 0 (info), 1 (warning), or
     2 or nil (error).  Defaults to nil.

     This is used by `flycheck-checker-pattern-to-error-regexp'
     to map error levels into `compilation-mode''s hierarchy and
     to get proper highlighting of errors in `compilation-mode'.

`:overlay-category CATEGORY'
     A symbol denoting the overlay category to use for error
     highlight overlays for this level.  See Info
     node `(elisp)Overlay Properties' for more information about
     overlay categories.

     A category for an error level overlay should at least define
     the `face' property, for error highlighting.  Another useful
     property for error level categories is `priority', to
     influence the stacking of multiple error level overlays.

`:fringe-bitmap BITMAPS'
     A fringe bitmap symbol denoting the bitmap to use for fringe
     indicators for this level, or a cons of two bitmaps (one for
     narrow fringes and one for wide fringes).  See Info node
     `(elisp)Fringe Bitmaps' for more information about fringe
     bitmaps, including a list of built-in fringe bitmaps.

`:fringe-face FACE'
     A face symbol denoting the face to use for fringe indicators
     for this level.

`:margin-spec SPEC'
     A display specification indicating what to display in the
     margin when `flycheck-indication-mode' is `left-margin' or
     `right-margin'.  See Info node `(elisp)Displaying in the
     Margins'.  If omitted, Flycheck generates an image spec from
     the fringe bitmap.

`:error-list-face FACE'
     A face symbol denoting the face to use for messages of this
     level in the error list.  See `flycheck-list-errors'.

(fn LEVEL &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-error-level 'lisp-indent-function '1) (autoload 'flycheck-define-command-checker "flycheck" "Define SYMBOL as syntax checker to run a command.

Define SYMBOL as generic syntax checker via
`flycheck-define-generic-checker', which uses an external command
to check the buffer.  SYMBOL and DOCSTRING are the same as for
`flycheck-define-generic-checker'.

In addition to the properties understood by
`flycheck-define-generic-checker', the following PROPERTIES
constitute a command syntax checker.  Unless otherwise noted, all
properties are mandatory.  Note that the default `:error-filter'
of command checkers is `flycheck-sanitize-errors'.

`:command COMMAND'
     The command to run for syntax checking.

     COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.

     Each ARG is an argument to the executable, either as string,
     or as special symbol or form for
     `flycheck-substitute-argument', which see.

`:error-patterns PATTERNS'
     A list of patterns to parse the output of the `:command'.

     Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
     LEVEL is a Flycheck error level (see
     `flycheck-define-error-level'), followed by one or more RX
     `SEXP's which parse an error of that level and extract line,
     column, file name and the message.

     See `rx' for general information about RX, and
     `flycheck-rx-to-string' for some special RX forms provided
     by Flycheck.

     All patterns are applied in the order of declaration to the
     whole output of the syntax checker.  Output already matched
     by a pattern will not be matched by subsequent patterns.  In
     other words, the first pattern wins.

     This property is optional.  If omitted, however, an
     `:error-parser' is mandatory.

`:error-parser FUNCTION'
     A function to parse errors with.

     The function shall accept three arguments OUTPUT CHECKER
     BUFFER.  OUTPUT is the syntax checker output as string,
     CHECKER the syntax checker that was used, and BUFFER a
     buffer object representing the checked buffer.  The function
     must return a list of `flycheck-error' objects parsed from
     OUTPUT.

     This property is optional.  If omitted, it defaults to
     `flycheck-parse-with-patterns'.  In this case,
     `:error-patterns' is mandatory.

`:standard-input t'
     Whether to send the buffer contents on standard input.

     If this property is given and has a non-nil value, send the
     contents of the buffer on standard input.

     Defaults to nil.

Note that you may not give `:start', `:interrupt', and
`:print-doc' for a command checker.  You can give a custom
`:verify' function, though, whose results will be appended to the
default `:verify' function of command checkers.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil nil) (function-put 'flycheck-define-command-checker 'lisp-indent-function '1) (function-put 'flycheck-define-command-checker 'doc-string-elt '2) (autoload 'flycheck-def-config-file-var "flycheck" "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

SYMBOL is declared as customizable variable using `defcustom', to
provide configuration files for the given syntax CHECKER.
CUSTOM-ARGS are forwarded to `defcustom'.

FILE-NAME is the initial value of the new variable.  If omitted,
the default value is nil.  It can be either a string or a list of
strings.

Use this together with the `config-file' form in the `:command'
argument to `flycheck-define-checker'.

(fn SYMBOL CHECKER &optional FILE-NAME &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-config-file-var 'lisp-indent-function '3) (autoload 'flycheck-def-option-var "flycheck" "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

SYMBOL is declared as customizable variable using `defcustom', to
provide an option for the given syntax CHECKERS (a checker or a
list of checkers).  INIT-VALUE is the initial value of the
variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
forwarded to `defcustom'.

Use this together with the `option', `option-list' and
`option-flag' forms in the `:command' argument to
`flycheck-define-checker'.

(fn SYMBOL INIT-VALUE CHECKERS DOCSTRING &rest CUSTOM-ARGS)" nil t) (function-put 'flycheck-def-option-var 'lisp-indent-function '3) (function-put 'flycheck-def-option-var 'doc-string-elt '4) (autoload 'flycheck-define-checker "flycheck" "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

Like `flycheck-define-command-checker', but PROPERTIES must not
be quoted.  Also, implicitly define the executable variable for
SYMBOL with `flycheck-def-executable-var'.

(fn SYMBOL DOCSTRING &rest PROPERTIES)" nil t) (function-put 'flycheck-define-checker 'lisp-indent-function '1) (function-put 'flycheck-define-checker 'doc-string-elt '2) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck" '("flycheck-" "help-flycheck-checker-d" "list-flycheck-errors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-buttercup" '("flycheck-buttercup-format-error-list"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-ert" '("flycheck-er"))) (provide 'flycheck-autoloads)) "flycheck-irony" ((flycheck-irony flycheck-irony-autoloads) (autoload 'flycheck-irony-setup "flycheck-irony" "Setup Flycheck Irony.

Add `irony' to `flycheck-checkers'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-irony" '("flycheck-irony-"))) (provide 'flycheck-irony-autoloads)) "typescript-mode" ((typescript-mode-autoloads typescript-mode-test-utilities typescript-mode) (put 'typescript-indent-level 'safe-local-variable #'integerp) (autoload 'typescript-mode "typescript-mode" "Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

(fn)" t nil) (eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}"))) (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typescript-mode" '("typescript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typescript-mode-test-utilities" '("font-lock-test" "get-face-at" "test-with-"))) (provide 'typescript-mode-autoloads)) "tide" ((tide-lv tide-autoloads tide) (autoload 'company-tide "tide" "

(fn COMMAND &optional ARG &rest IGNORED)" t nil) (autoload 'tide-format-before-save "tide" "Before save hook to format the buffer before each save." t nil) (autoload 'tide-format "tide" "Format the current region or buffer." t nil) (autoload 'tide-setup "tide" "Setup `tide-mode' in current buffer." t nil) (autoload 'tide-mode "tide" "Minor mode for Typescript Interactive Development Environment.

If called interactively, enable Tide mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\\{tide-mode-map}

(fn &optional ARG)" t nil) (autoload 'tide-project-errors "tide" nil t nil) (autoload 'tide-unhighlight-identifiers "tide" "Remove highlights from previously highlighted identifier." nil nil) (autoload 'tide-hl-identifier "tide" "Highlight all instances of the identifier under point. Removes
highlights from previously highlighted identifier." t nil) (autoload 'tide-hl-identifier-mode "tide" "Highlight instances of the identifier at point after a short
timeout.

If called interactively, enable Tide-Hl-Identifier mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tide" '("tide-" "xref-tide-xref-backend"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tide-lv" '("tide-lv-"))) (provide 'tide-autoloads)) "rust-mode" ((rust-compile rust-cargo rust-playpen rust-utils rust-rustfmt rust-mode rust-mode-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-cargo" '("rust-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-compile" '("cargo-compilation-regexps" "rustc-"))) (autoload 'rust-mode "rust-mode" "Major mode for Rust code.

\\{rust-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-mode" '("rust-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-playpen" '("rust-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-rustfmt" '("rust-"))) (autoload 'rust-dbg-wrap-or-unwrap "rust-utils" "Either remove or add the dbg! macro." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-utils" '("rust-"))) (provide 'rust-mode-autoloads)) "f" ((f f-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "f" '("f-"))) (provide 'f-autoloads)) "markdown-mode" ((markdown-mode markdown-mode-autoloads) (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files.

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode)) (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files.

(fn)" t nil) (autoload 'markdown-view-mode "markdown-mode" "Major mode for viewing Markdown content.

(fn)" t nil) (autoload 'gfm-view-mode "markdown-mode" "Major mode for viewing GitHub Flavored Markdown content.

(fn)" t nil) (autoload 'markdown-live-preview-mode "markdown-mode" "Toggle native previewing on save for a specific markdown file.

If called interactively, enable Markdown-Live-Preview mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "markdown-mode" '("defun-markdown-" "gfm-" "markdown"))) (provide 'markdown-mode-autoloads)) "xref" ((xref-autoloads xref) (autoload 'xref-find-backend "xref" nil nil nil) (define-obsolete-function-alias 'xref-pop-marker-stack #'xref-go-back "29.1") (autoload 'xref-go-back "xref" "Go back to the previous position in xref history.
To undo, use \\[xref-go-forward]." t nil) (autoload 'xref-go-forward "xref" "Got to the point where a previous \\[xref-go-back] was invoked." t nil) (autoload 'xref-marker-stack-empty-p "xref" "Whether the xref back-history is empty." nil nil) (autoload 'xref-forward-history-empty-p "xref" "Whether the xref forward-history is empty." nil nil) (autoload 'xref-find-definitions "xref" "Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it.

If sufficient information is available to determine a unique
definition for IDENTIFIER, display it in the selected window.
Otherwise, display the list of the possible definitions in a
buffer where the user can select from the list.

Use \\[xref-go-back] to return back to where you invoked this command.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-definitions-other-window "xref" "Like `xref-find-definitions' but switch to the other window.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-definitions-other-frame "xref" "Like `xref-find-definitions' but switch to the other frame.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-references "xref" "Find references to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point.

(fn IDENTIFIER)" t nil) (autoload 'xref-find-definitions-at-mouse "xref" "Find the definition of identifier at or around mouse click.
This command is intended to be bound to a mouse event.

(fn EVENT)" t nil) (autoload 'xref-find-references-at-mouse "xref" "Find references to the identifier at or around mouse click.
This command is intended to be bound to a mouse event.

(fn EVENT)" t nil) (autoload 'xref-find-apropos "xref" "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'.
See `tags-apropos-additional-actions' for how to augment the
output of this command when the backend is etags.

(fn PATTERN)" t nil) (define-key esc-map "." #'xref-find-definitions) (define-key esc-map "," #'xref-go-back) (define-key esc-map [67108908] #'xref-go-forward) (define-key esc-map "?" #'xref-find-references) (define-key esc-map [67108910] #'xref-find-apropos) (define-key ctl-x-4-map "." #'xref-find-definitions-other-window) (define-key ctl-x-5-map "." #'xref-find-definitions-other-frame) (autoload 'xref-references-in-directory "xref" "Find all references to SYMBOL in directory DIR.
Return a list of xref values.

This function uses the Semantic Symbol Reference API, see
`semantic-symref-tool-alist' for details on which tools are used,
and when.

(fn SYMBOL DIR)" nil nil) (autoload 'xref-matches-in-directory "xref" "Find all matches for REGEXP in directory DIR.
Return a list of xref values.
Only files matching some of FILES and none of IGNORES are searched.
FILES is a string with glob patterns separated by spaces.
IGNORES is a list of glob patterns for files to ignore.

(fn REGEXP FILES DIR IGNORES)" nil nil) (autoload 'xref-matches-in-files "xref" "Find all matches for REGEXP in FILES.
Return a list of xref values.
FILES must be a list of absolute file names.

See `xref-search-program' and `xref-search-program-alist' for how
to control which program to use when looking for matches.

(fn REGEXP FILES)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xref" '("xref-"))) (provide 'xref-autoloads)) "project" ((project project-autoloads) (autoload 'project-current "project" "Return the project instance in DIRECTORY, defaulting to `default-directory'.

When no project is found in that directory, the result depends on
the value of MAYBE-PROMPT: if it is nil or omitted, return nil,
else ask the user for a directory in which to look for the
project, and if no project is found there, return a \"transient\"
project instance.

The \"transient\" project instance is a special kind of value
which denotes a project rooted in that directory and includes all
the files under the directory except for those that should be
ignored (per `project-ignores').

See the doc string of `project-find-functions' for the general form
of the project instance object.

(fn &optional MAYBE-PROMPT DIRECTORY)" nil nil) (defvar project-prefix-map (let ((map (make-sparse-keymap))) (define-key map "!" 'project-shell-command) (define-key map "&" 'project-async-shell-command) (define-key map "f" 'project-find-file) (define-key map "F" 'project-or-external-find-file) (define-key map "b" 'project-switch-to-buffer) (define-key map "s" 'project-shell) (define-key map "d" 'project-find-dir) (define-key map "D" 'project-dired) (define-key map "v" 'project-vc-dir) (define-key map "c" 'project-compile) (define-key map "e" 'project-eshell) (define-key map "k" 'project-kill-buffers) (define-key map "p" 'project-switch-project) (define-key map "g" 'project-find-regexp) (define-key map "G" 'project-or-external-find-regexp) (define-key map "r" 'project-query-replace-regexp) (define-key map "x" 'project-execute-extended-command) map) "Keymap for project commands.") (define-key ctl-x-map "p" project-prefix-map) (autoload 'project-other-window-command "project" "Run project command, displaying resultant buffer in another window.

The following commands are available:

\\{project-prefix-map}
\\{project-other-window-map}" t nil) (define-key ctl-x-4-map "p" #'project-other-window-command) (autoload 'project-other-frame-command "project" "Run project command, displaying resultant buffer in another frame.

The following commands are available:

\\{project-prefix-map}
\\{project-other-frame-map}" t nil) (define-key ctl-x-5-map "p" #'project-other-frame-command) (autoload 'project-other-tab-command "project" "Run project command, displaying resultant buffer in a new tab.

The following commands are available:

\\{project-prefix-map}" t nil) (when (bound-and-true-p tab-prefix-map) (define-key tab-prefix-map "p" #'project-other-tab-command)) (autoload 'project-find-regexp "project" "Find all matches for REGEXP in the current project's roots.
With \\[universal-argument] prefix, you can specify the directory
to search in, and the file name pattern to search for.  The
pattern may use abbreviations defined in `grep-files-aliases',
e.g. entering `ch' is equivalent to `*.[ch]'.  As whitespace
triggers completion when entering a pattern, including it
requires quoting, e.g. `\\[quoted-insert]<space>'.

(fn REGEXP)" t nil) (autoload 'project-or-external-find-regexp "project" "Find all matches for REGEXP in the project roots or external roots.
With \\[universal-argument] prefix, you can specify the file name
pattern to search for.

(fn REGEXP)" t nil) (autoload 'project-find-file "project" "Visit a file (with completion) in the current project.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'.

(fn &optional INCLUDE-ALL)" t nil) (autoload 'project-or-external-find-file "project" "Visit a file (with completion) in the current project or external roots.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'.

(fn &optional INCLUDE-ALL)" t nil) (autoload 'project-find-dir "project" "Start Dired in a directory inside the current project." t nil) (autoload 'project-dired "project" "Start Dired in the current project's root." t nil) (autoload 'project-vc-dir "project" "Run VC-Dir in the current project's root." t nil) (autoload 'project-shell "project" "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists." t nil) (autoload 'project-eshell "project" "Start Eshell in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists." t nil) (autoload 'project-async-shell-command "project" "Run `async-shell-command' in the current project's root directory." t nil) (function-put 'project-async-shell-command 'interactive-only 'async-shell-command) (autoload 'project-shell-command "project" "Run `shell-command' in the current project's root directory." t nil) (function-put 'project-shell-command 'interactive-only 'shell-command) (autoload 'project-search "project" "Search for REGEXP in all the files of the project.
Stops when a match is found.
To continue searching for the next match, use the
command \\[fileloop-continue].

(fn REGEXP)" t nil) (autoload 'project-query-replace-regexp "project" "Query-replace REGEXP in all the files of the project.
Stops when a match is found and prompts for whether to replace it.
If you exit the `query-replace', you can later continue the
`query-replace' loop using the command \\[fileloop-continue].

(fn FROM TO)" t nil) (autoload 'project-compile "project" "Run `compile' in the project root." t nil) (function-put 'project-compile 'interactive-only 'compile) (autoload 'project-switch-to-buffer "project" "Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

(fn BUFFER-OR-NAME)" t nil) (autoload 'project-display-buffer "project" "Display BUFFER-OR-NAME in some window, without selecting it.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer' as a subroutine, which see
for how it is determined where the buffer will be displayed.

(fn BUFFER-OR-NAME)" t nil) (autoload 'project-display-buffer-other-frame "project" "Display BUFFER-OR-NAME preferably in another frame.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical.

This function uses `display-buffer-other-frame' as a subroutine,
which see for how it is determined where the buffer will be
displayed.

(fn BUFFER-OR-NAME)" t nil) (autoload 'project-kill-buffers "project" "Kill the buffers belonging to the current project.
Two buffers belong to the same project if their project
instances, as reported by `project-current' in each buffer, are
identical.  Only the buffers that match a condition in
`project-kill-buffer-conditions' will be killed.  If NO-CONFIRM
is non-nil, the command will not ask the user for confirmation.
NO-CONFIRM is always nil when the command is invoked
interactively.

(fn &optional NO-CONFIRM)" t nil) (autoload 'project-remember-project "project" "Add project PR to the front of the project list.
Save the result in `project-list-file' if the list of projects
has changed, and NO-WRITE is nil.

(fn PR &optional NO-WRITE)" nil nil) (autoload 'project-forget-project "project" "Remove directory PROJECT-ROOT from the project list.
PROJECT-ROOT is the root directory of a known project listed in
the project list.

(fn PROJECT-ROOT)" t nil) (autoload 'project-known-project-roots "project" "Return the list of root directories of all known projects." nil nil) (autoload 'project-execute-extended-command "project" "Execute an extended command in project root." t nil) (function-put 'project-execute-extended-command 'interactive-only 'command-execute) (autoload 'project-switch-project "project" "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR.

(fn DIR)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "project" '("project-"))) (provide 'project-autoloads)) "spinner" ((spinner-autoloads spinner) (autoload 'spinner-create "spinner" "Create a spinner of the given TYPE.
The possible TYPEs are described in `spinner--type-to-frames'.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

If BUFFER-LOCAL is non-nil, the spinner will be automatically
deactivated if the buffer is killed.  If BUFFER-LOCAL is a
buffer, use that instead of current buffer.

When started, in order to function properly, the spinner runs a
timer which periodically calls `force-mode-line-update' in the
current buffer.  If BUFFER-LOCAL was set at creation time, then
`force-mode-line-update' is called in that buffer instead.  When
the spinner is stopped, the timer is deactivated.

DELAY, if given, is the number of seconds to wait after starting
the spinner before actually displaying it. It is safe to cancel
the spinner before this time, in which case it won't display at
all.

(fn &optional TYPE BUFFER-LOCAL FPS DELAY)" nil nil) (autoload 'spinner-start "spinner" "Start a mode-line spinner of given TYPE-OR-OBJECT.
If TYPE-OR-OBJECT is an object created with `make-spinner',
simply activate it.  This method is designed for minor modes, so
they can use the spinner as part of their lighter by doing:
    '(:eval (spinner-print THE-SPINNER))
To stop this spinner, call `spinner-stop' on it.

If TYPE-OR-OBJECT is anything else, a buffer-local spinner is
created with this type, and it is displayed in the
`mode-line-process' of the buffer it was created it.  Both
TYPE-OR-OBJECT and FPS are passed to `make-spinner' (which see).
To stop this spinner, call `spinner-stop' in the same buffer.

Either way, the return value is a function which can be called
anywhere to stop this spinner.  You can also call `spinner-stop'
in the same buffer where the spinner was created.

FPS, if given, is the number of desired frames per second.
Default is `spinner-frames-per-second'.

DELAY, if given, is the number of seconds to wait until actually
displaying the spinner. It is safe to cancel the spinner before
this time, in which case it won't display at all.

(fn &optional TYPE-OR-OBJECT FPS DELAY)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spinner" '("spinner-"))) (provide 'spinner-autoloads)) "xterm-color" ((xterm-color-autoloads xterm-color) (autoload 'xterm-color-filter-strip "xterm-color" "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

In order to get maximum performance, this function strips text properties
if they are present in STRING.

(fn STRING)" nil nil) (autoload 'xterm-color-filter "xterm-color" "Translate ANSI color sequences in STRING into text properties.
Return new STRING with text properties applied.

This function checks if `xterm-color-preserve-properties' is non-nil
and only calls `xterm-color-filter-strip' on substrings that do not
have text properties applied (passing through the rest unmodified).
Preserving properties in this fashion is not very robust as there may
be situations where text properties are applied on ANSI data, which
will desync the state machine.

Preserving properties works ok with and is really meant for eshell.

This can be inserted into `comint-preoutput-filter-functions'.

(fn STRING)" nil nil) (autoload 'xterm-color-256 "xterm-color" "

(fn COLOR)" nil nil) (autoload 'xterm-color-colorize-buffer "xterm-color" "Apply `xterm-color-filter' to current buffer, and replace its contents.
Colors are applied using 'face, unless font-lock-mode is active, in
which case 'font-lock-face is used. Operation with font-lock mode active
is not recommended.

If USE-OVERLAYS is non-nil, colors are applied to the buffer using overlays
instead of text properties. A C-u prefix arg causes overlays to be used.

(fn &optional USE-OVERLAYS)" t nil) (autoload 'xterm-color-clear-cache "xterm-color" "Clear xterm color face attribute cache.
You may want to call this if you change `xterm-color-names' or
`xterm-color-names-bright' at runtime and you want to see the changes
take place in a pre-existing buffer that has had xterm-color initialized.

Since the cache is buffer-local and created on-demand when needed, this has no
effect when called from a buffer that does not have a cache." t nil) (autoload 'xterm-color-test "xterm-color" "Create, display and render a new buffer containing ANSI control sequences." t nil) (autoload 'xterm-color-test-raw "xterm-color" "Create and display a new buffer containing ANSI SGR control sequences.
ANSI sequences are not processed. One can use a different Emacs package,
such as ansi-color.el to do so. This is really meant to be used for easy
comparisons/benchmarks with libraries that offer similar functionality." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xterm-color" '("+xterm-color--table-256+" "xterm-color-"))) (provide 'xterm-color-autoloads)) "rustic" ((rustic-compile rustic-interaction rustic-racer rustic-playpen rustic-babel rustic rustic-popup rustic-flycheck rustic-rustfix rustic-lsp rustic-autoloads rustic-cargo rustic-doc rustic-rustfmt) (autoload 'rustic-mode "rustic" "Major mode for Rust code.

\\{rustic-mode-map}

(fn)" t nil) (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic" '("rustic-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-babel" '("cargo-toml-dependencies" "crate-dependencies" "org-babel-execute:rust" "rustic-"))) (autoload 'rustic-cargo-clippy-run "rustic-cargo" "Run `cargo clippy' with optional ARGS.

(fn &optional ARGS)" t nil) (autoload 'rustic-cargo-clippy "rustic-cargo" "Run 'cargo clippy'.

If ARG is not nil, use value as argument and store it in `rustic-clippy-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-clippy-arguments'.

(fn &optional ARG)" t nil) (autoload 'rustic-cargo-clippy-rerun "rustic-cargo" "Run 'cargo clippy' with `rustic-clippy-arguments'." t nil) (autoload 'rustic-cargo-test-run "rustic-cargo" "Start compilation process for 'cargo test' with optional TEST-ARGS.

(fn &optional TEST-ARGS)" t nil) (autoload 'rustic-cargo-test "rustic-cargo" "Run 'cargo test'.

If ARG is not nil, use value as argument and store it in `rustic-test-arguments'.
When calling this function from `rustic-popup-mode', always use the value of
`rustic-test-arguments'.

(fn &optional ARG)" t nil) (autoload 'rustic-cargo-test-rerun "rustic-cargo" "Run 'cargo test' with `rustic-test-arguments'." t nil) (autoload 'rustic-cargo-current-test "rustic-cargo" "Run 'cargo test' for the test near point." t nil) (autoload 'rustic-cargo-outdated "rustic-cargo" "Use 'cargo outdated' to list outdated packages in `tabulated-list-mode'.
Execute process in PATH.

(fn &optional PATH)" t nil) (autoload 'rustic-cargo-reload-outdated "rustic-cargo" "Update list of outdated packages." t nil) (autoload 'rustic-cargo-mark-upgrade "rustic-cargo" "Mark an upgradable package." t nil) (autoload 'rustic-cargo-mark-all-upgrades "rustic-cargo" "Mark all upgradable packages in the Package Menu." t nil) (autoload 'rustic-cargo-menu-mark-unmark "rustic-cargo" "Clear any marks on a package." t nil) (autoload 'rustic-cargo-upgrade-execute "rustic-cargo" "Perform marked menu actions." t nil) (autoload 'rustic-cargo-new "rustic-cargo" "Run 'cargo new' to start a new package in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library.

(fn PROJECT-PATH &optional BIN)" t nil) (autoload 'rustic-cargo-init "rustic-cargo" "Run 'cargo init' to initialize a directory in the path specified by PROJECT-PATH.
If BIN is not nil, create a binary application, otherwise a library.

(fn PROJECT-PATH &optional BIN)" t nil) (autoload 'rustic-cargo-build "rustic-cargo" "Run 'cargo build' for the current project." t nil) (autoload 'rustic-run-shell-command "rustic-cargo" "Run an arbitrary shell command for the current project.
Example: use it to provide an environment variable to your application like this `env MYVAR=1 cargo run' so that it can read it at the runtime.
As a byproduct, you can run any shell command in your project like `pwd'

(fn &optional ARG)" t nil) (autoload 'rustic-cargo-run "rustic-cargo" "Run 'cargo run' for the current project.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t nil) (autoload 'rustic-cargo-clean "rustic-cargo" "Run 'cargo clean' for the current project." t nil) (autoload 'rustic-cargo-check "rustic-cargo" "Run 'cargo check' for the current project." t nil) (autoload 'rustic-cargo-bench "rustic-cargo" "Run 'cargo bench' for the current project." t nil) (autoload 'rustic-cargo-build-doc "rustic-cargo" "Build the documentation for the current project." t nil) (autoload 'rustic-cargo-doc "rustic-cargo" "Open the documentation for the current project in a browser.
The documentation is built if necessary." t nil) (autoload 'rustic-cargo-add "rustic-cargo" "Add crate to Cargo.toml using 'cargo add'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t nil) (autoload 'rustic-cargo-rm "rustic-cargo" "Remove crate from Cargo.toml using 'cargo rm'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t nil) (autoload 'rustic-cargo-upgrade "rustic-cargo" "Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'.
If running with prefix command `C-u', read whole command from minibuffer.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-cargo" '("poly-rustic-cargo-comint-switch-buffer-hook" "rustic-"))) (autoload 'rustic-compile "rustic-compile" "Compile rust project.

If `compilation-read-command' is non-nil or if called with prefix
argument ARG then read the command in the minibuffer.  Otherwise
use `rustic-compile-command'.

In either store the used command in `compilation-arguments'.

(fn &optional ARG)" t nil) (autoload 'rustic-recompile "rustic-compile" "Re-compile the program using `compilation-arguments'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-compile" '("rust"))) (autoload 'rustic-doc-dumb-search "rustic-doc" "Search all projects and std for SEARCH-TERM.
Use this when `rustic-doc-search' does not find what you're looking for.
Add `universal-argument' to only search level 1 headers.
See `rustic-doc-search' for more information.

(fn SEARCH-TERM)" t nil) (autoload 'rustic-doc-search "rustic-doc" "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results.
To limit search results to only level 1 headers, add `universal-argument'
Level 1 headers are things like struct or enum names.
if ROOT is non-nil the search is performed from the root dir.
This function tries to be smart and limits the search results
as much as possible. If it ends up being so smart that
it doesn't manage to find what you're looking for, try `rustic-doc-dumb-search'.

(fn SEARCH-TERM &optional ROOT)" t nil) (autoload 'rustic-doc-convert-current-package "rustic-doc" "Convert the documentation for a project and its dependencies." t nil) (autoload 'rustic-doc-setup "rustic-doc" "Setup or update rustic-doc filter and convert script. Convert std." t nil) (autoload 'rustic-doc-mode "rustic-doc" "Convert rust html docs to .org, and browse the converted docs.

If called interactively, enable Rustic-Doc mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-doc" '("rustic-doc-"))) (autoload 'rustic-flycheck-setup "rustic-flycheck" "Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-flycheck" '("rustic-flycheck-"))) (autoload 'rustic-open-dependency-file "rustic-interaction" "Open the 'Cargo.toml' file at the project root if the current buffer is
visiting a project." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-interaction" '("rustic-"))) (autoload 'rustic-analyzer-macro-expand "rustic-lsp" "Default method for displaying macro expansion results.

(fn RESULT)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-lsp" '("rustic-"))) (autoload 'rustic-playpen "rustic-playpen" "Create a shareable URL for the contents of the current region,
src-block or buffer on the Rust playpen.

(fn BEGIN END)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-playpen" '("rustic-"))) (autoload 'rustic-popup "rustic-popup" "Setup popup.
If directory is not in a rust project call `read-directory-name'." t nil) (autoload 'rustic-popup-invoke-popup-action "rustic-popup" "Execute commands which are listed in `rustic-popup-commands'.

(fn EVENT)" t nil) (autoload 'rustic-popup-default-action "rustic-popup" "Change backtrace and `compilation-arguments' when executed on
corresponding line." t nil) (autoload 'rustic-popup-cargo-command-help "rustic-popup" "Display help buffer for cargo command at point." t nil) (autoload 'rustic-popup-kill-help-buffer "rustic-popup" "Kill popup help buffer and switch to popup buffer." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-popup" '("rustic-"))) (autoload 'rustic-racer-describe "rustic-racer" "Show a *Racer Help* buffer for the function or type at point." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-racer" '("racer-src-button" "rustic-racer-"))) (autoload 'rustic-rustfix "rustic-rustfix" "Run 'cargo fix'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-rustfix" '("rustic-rustfix-"))) (autoload 'rustic-cargo-fmt "rustic-rustfmt" "Use rustfmt via cargo." t nil) (autoload 'rustic-format-region "rustic-rustfmt" "Format the current active region using rustfmt.

This operation requires a nightly version of rustfmt.

(fn BEGIN END)" t nil) (autoload 'rustic-format-buffer "rustic-rustfmt" "Format the current buffer using rustfmt.

Provide optional argument NO-STDIN for `rustic-before-save-hook' since there
were issues when using stdin for formatting." t nil) (autoload 'rustic-format-file "rustic-rustfmt" "Unlike `rustic-format-buffer' format file directly and revert the buffer.

(fn &optional FILE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-rustfmt" '("rustic-"))) (provide 'rustic-autoloads)) "ht" ((ht-autoloads ht) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ht" 'nil)) (provide 'ht-autoloads)) "lv" ((lv-autoloads lv) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lv" '("lv-"))) (provide 'lv-autoloads)) "lsp-mode" ((lsp-gdscript lsp-diagnostics lsp-racket lsp-vimscript lsp-php lsp-tex lsp-mode lsp-css lsp-mode-autoloads lsp-vetur lsp-protocol lsp-xml lsp-completion lsp-pylsp lsp-zig lsp-ocaml lsp-json lsp lsp-clangd lsp-kotlin lsp-lua lsp-dired lsp-semantic-tokens lsp-prolog lsp-hack lsp-iedit lsp-lens lsp-headerline lsp-yaml lsp-eslint lsp-ada lsp-nginx lsp-javascript lsp-nix lsp-solargraph lsp-sorbet lsp-v lsp-clojure lsp-nim lsp-fsharp lsp-toml lsp-groovy lsp-dhall lsp-vhdl lsp-go lsp-erlang lsp-verilog lsp-terraform lsp-beancount lsp-fortran lsp-bash lsp-actionscript lsp-modeline lsp-rust lsp-graphql lsp-icons lsp-ido lsp-d lsp-pwsh lsp-r lsp-crystal lsp-purescript lsp-angular lsp-html lsp-elm lsp-sqls lsp-steep lsp-haxe lsp-pyls lsp-markdown lsp-csharp lsp-svelte lsp-dockerfile lsp-perl lsp-elixir lsp-cmake lsp-vala lsp-rf) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-actionscript" '("lsp-actionscript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ada" '("lsp-ada-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-angular" '("lsp-client"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-bash" '("lsp-bash-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-beancount" '("lsp-beancount-"))) (autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-clangd" "Explain a clang-tidy ERROR by scraping documentation from llvm.org.

(fn ERROR)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clangd" '("lsp-c"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-clojure" '("lsp-clojure-"))) (define-obsolete-variable-alias 'lsp-prefer-capf 'lsp-completion-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-enable-completion-at-point 'lsp-completion-enable "lsp-mode 7.0.1") (autoload 'lsp-completion-at-point "lsp-completion" "Get lsp completions." nil nil) (autoload 'lsp-completion--enable "lsp-completion" "Enable LSP completion support." nil nil) (autoload 'lsp-completion-mode "lsp-completion" "Toggle LSP completion support.

If called interactively, enable Lsp-Completion mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when (and lsp-auto-configure lsp-completion-enable) (lsp-completion--enable)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-completion" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-crystal" '("lsp-clients-crystal-executable"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-csharp" '("lsp-csharp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-css" '("lsp-css-"))) (define-obsolete-variable-alias 'lsp-diagnostic-package 'lsp-diagnostics-provider "lsp-mode 7.0.1") (define-obsolete-variable-alias 'lsp-flycheck-default-level 'lsp-diagnostics-flycheck-default-level "lsp-mode 7.0.1") (autoload 'lsp-diagnostics-lsp-checker-if-needed "lsp-diagnostics" nil nil nil) (autoload 'lsp-diagnostics--enable "lsp-diagnostics" "Enable LSP checker support." nil nil) (autoload 'lsp-diagnostics-mode "lsp-diagnostics" "Toggle LSP diagnostics integration.

If called interactively, enable Lsp-Diagnostics mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (add-hook 'lsp-configure-hook (lambda nil (when lsp-auto-configure (lsp-diagnostics--enable)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-diagnostics" '("lsp-diagnostics-"))) (defvar lsp-dired-mode nil "Non-nil if Lsp-Dired mode is enabled.
See the `lsp-dired-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `lsp-dired-mode'.") (custom-autoload 'lsp-dired-mode "lsp-dired" nil) (autoload 'lsp-dired-mode "lsp-dired" "Display `lsp-mode' icons for each file in a dired buffer.

If called interactively, enable Lsp-Dired mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dired" '("lsp-dired-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-dockerfile" '("lsp-dockerfile-language-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elixir" '("lsp-elixir-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-elm" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-erlang" '("lsp-erlang-server-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-eslint" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fortran" '("lsp-clients-"))) (autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "Load all of the provided PROJECTS.

(fn PROJECTS)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-fsharp" '("lsp-fsharp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-gdscript" '("lsp-gdscript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-go" '("lsp-go-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-graphql" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-groovy" '("lsp-groovy-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-hack" '("lsp-clients-hack-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-haxe" '("lsp-"))) (autoload 'lsp-headerline-breadcrumb-mode "lsp-headerline" "Toggle breadcrumb on headerline.

If called interactively, enable Lsp-Headerline-Breadcrumb mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-breadcrumb-go-to-symbol "lsp-headerline" "Go to the symbol on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (autoload 'lsp-breadcrumb-narrow-to-symbol "lsp-headerline" "Narrow to the symbol range on breadcrumb at SYMBOL-POSITION.

(fn SYMBOL-POSITION)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-headerline" '("lsp-headerline-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-html" '("lsp-html-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-icons" '("lsp-"))) (autoload 'lsp-ido-workspace-symbol "lsp-ido" "`ido' for lsp workspace/symbol.
When called with prefix ARG the default selection will be symbol at point.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ido" '("lsp-ido-"))) (autoload 'lsp-iedit-highlights "lsp-iedit" "Start an `iedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (autoload 'lsp-iedit-linked-ranges "lsp-iedit" "Start an `iedit' for `textDocument/linkedEditingRange'" t nil) (autoload 'lsp-evil-multiedit-highlights "lsp-iedit" "Start an `evil-multiedit' operation on the documentHighlights at point.
This can be used as a primitive `lsp-rename' replacement if the
language server doesn't support renaming.

See also `lsp-enable-symbol-highlighting'." t nil) (autoload 'lsp-evil-multiedit-linked-ranges "lsp-iedit" "Start an `evil-multiedit' for `textDocument/linkedEditingRange'" t nil) (autoload 'lsp-evil-state-highlights "lsp-iedit" "Start `iedit-mode'. for `textDocument/documentHighlight'" t nil) (autoload 'lsp-evil-state-linked-ranges "lsp-iedit" "Start `iedit-mode'. for `textDocument/linkedEditingRange'" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-iedit" '("lsp-iedit--on-ranges"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-javascript" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-json" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-kotlin" '("lsp-"))) (autoload 'lsp-lens--enable "lsp-lens" "Enable lens mode." nil nil) (autoload 'lsp-lens-show "lsp-lens" "Display lenses in the buffer." t nil) (autoload 'lsp-lens-hide "lsp-lens" "Delete all lenses." t nil) (autoload 'lsp-lens-mode "lsp-lens" "Toggle code-lens overlays.

If called interactively, enable Lsp-Lens mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-avy-lens "lsp-lens" "Click lsp lens using `avy' package." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lens" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-lua" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-markdown" '("lsp-markdown-"))) (put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp) (put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i)))) (autoload 'lsp-load-vscode-workspace "lsp-mode" "Load vscode workspace from FILE

(fn FILE)" t nil) (autoload 'lsp-save-vscode-workspace "lsp-mode" "Save vscode workspace to FILE

(fn FILE)" t nil) (autoload 'lsp-install-server "lsp-mode" "Interactively install or re-install server.
When prefix UPDATE? is t force installation even if the server is present.

(fn UPDATE\\=\\? &optional SERVER-ID)" t nil) (autoload 'lsp-update-server "lsp-mode" "Interactively update a server.

(fn &optional SERVER-ID)" t nil) (autoload 'lsp-ensure-server "lsp-mode" "Ensure server SERVER-ID

(fn SERVER-ID)" nil nil) (autoload 'lsp "lsp-mode" "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

(fn &optional ARG)" t nil) (autoload 'lsp-deferred "lsp-mode" "Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-mode" '("defcustom-lsp" "lsp-" "make-lsp-client" "when-lsp-workspace" "with-lsp-workspace"))) (define-obsolete-variable-alias 'lsp-diagnostics-modeline-scope 'lsp-modeline-diagnostics-scope "lsp-mode 7.0.1") (autoload 'lsp-modeline-code-actions-mode "lsp-modeline" "Toggle code actions on modeline.

If called interactively, enable Lsp-Modeline-Code-Actions mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (define-obsolete-function-alias 'lsp-diagnostics-modeline-mode 'lsp-modeline-diagnostics-mode "lsp-mode 7.0.1") (autoload 'lsp-modeline-diagnostics-mode "lsp-modeline" "Toggle diagnostics modeline.

If called interactively, enable Lsp-Modeline-Diagnostics mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'lsp-modeline-workspace-status-mode "lsp-modeline" "Toggle workspace status on modeline.

If called interactively, enable Lsp-Modeline-Workspace-Status
mode if ARG is positive, and disable it if ARG is zero or
negative.  If called from Lisp, also enable the mode if ARG is
omitted or nil, and toggle it if ARG is `toggle'; disable the
mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-modeline" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nginx" '("lsp-nginx-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-nix" '("lsp-nix-server-path"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ocaml" '("lsp-ocaml-l"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-perl" '("lsp-perl-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-php" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-prolog" '("lsp-prolog-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-protocol" '("dash-expand:&RangeToPoint" "lsp"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-purescript" '("lsp-purescript-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pwsh" '("lsp-pwsh-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pyls" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-pylsp" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-r" '("lsp-clients-r-server-command"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-racket" '("lsp-racket-lang"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rf" '("expand-start-command" "lsp-rf-language-server-" "parse-rf-language-server-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-rust" '("lsp-"))) (autoload 'lsp--semantic-tokens-initialize-buffer "lsp-semantic-tokens" "Initialize the buffer for semantic tokens.
IS-RANGE-PROVIDER is non-nil when server supports range requests." nil nil) (autoload 'lsp--semantic-tokens-initialize-workspace "lsp-semantic-tokens" "Initialize semantic tokens for WORKSPACE.

(fn WORKSPACE)" nil nil) (autoload 'lsp-semantic-tokens--warn-about-deprecated-setting "lsp-semantic-tokens" "Warn about deprecated semantic highlighting variable." nil nil) (autoload 'lsp-semantic-tokens--enable "lsp-semantic-tokens" "Enable semantic tokens mode." nil nil) (autoload 'lsp-semantic-tokens-mode "lsp-semantic-tokens" "Toggle semantic-tokens support.

If called interactively, enable Lsp-Semantic-Tokens mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-semantic-tokens" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-solargraph" '("lsp-solargraph-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sorbet" '("lsp-sorbet-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-sqls" '("lsp-sql"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-steep" '("lsp-steep-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-svelte" '("lsp-svelte-plugin-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-terraform" '("lsp-terraform-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-tex" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-toml" '("lsp-toml-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-v" '("lsp-v-vls-executable"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vala" '("lsp-clients-vala-ls-executable"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-verilog" '("lsp-clients-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vetur" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vhdl" '("ghdl-ls-bin-name" "hdl-checker-bin-name" "lsp-vhdl-" "vhdl-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-vimscript" '("lsp-clients-vim-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-xml" '("lsp-xml-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-yaml" '("lsp-yaml-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-zig" '("lsp-zig-zls-executable"))) (provide 'lsp-mode-autoloads)) "lsp-ui" ((lsp-ui-autoloads lsp-ui-doc lsp-ui-imenu lsp-ui-util lsp-ui-peek lsp-ui-flycheck lsp-ui-sideline lsp-ui) (autoload 'lsp-ui-mode "lsp-ui" "Toggle language server UI mode on or off.
\342\200\230lsp-ui-mode\342\200\231 is a minor mode that contains a series of useful UI
integrations for \342\200\230lsp-mode\342\200\231.  With a prefix argument ARG, enable
language server UI mode if ARG is positive, and disable it
otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil, and toggle it if ARG is \342\200\230toggle\342\200\231.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui" '("lsp-ui-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-doc" '("lsp-ui-doc-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-flycheck" '("lsp-ui-flycheck-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-imenu" '("lsp-ui-imenu"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-peek" '("lsp-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-sideline" '("lsp-ui-sideline"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-ui-util" '("lsp-ui-util-"))) (provide 'lsp-ui-autoloads)) "all-the-icons" ((all-the-icons-faces all-the-icons all-the-icons-autoloads) (autoload 'all-the-icons-icon-for-dir "all-the-icons" "Get the formatted icon for DIR.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

Note: You want chevron, please use `all-the-icons-icon-for-dir-with-chevron'.

(fn DIR &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-icon-for-file "all-the-icons" "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn FILE &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-icon-for-mode "all-the-icons" "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn MODE &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-icon-for-url "all-the-icons" "Get the formatted icon for URL.
If an icon for URL isn't found in `all-the-icons-url-alist', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions.

(fn URL &rest ARG-OVERRIDES)" nil nil) (autoload 'all-the-icons-install-fonts "all-the-icons" "Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

(fn &optional PFX)" t nil) (autoload 'all-the-icons-insert "all-the-icons" "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it.

(fn &optional ARG FAMILY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "all-the-icons" '("all-the-icons-"))) (provide 'all-the-icons-autoloads)) "xelb" ((xcb-xtest xcb-xvmc xcb-bigreq xcb-xf86vidmode xcb-xproto xcb-shm xcb-render xcb-xselinux xcb-xf86dri xcb-composite xcb-xc_misc xelb-autoloads xcb-dri3 xcb-screensaver xcb-ewmh xcb-ge xelb xcb-keysyms xcb-systemtray xcb-xkb xcb-randr xcb xcb-xim xcb-sync xcb-dri2 xcb-renderutil xcb-icccm xcb-dpms xcb-debug xcb-xinput xcb-damage xcb-xembed xcb-record xcb-types xcb-xv el_client xcb-present xcb-glx xcb-xinerama xcb-xlib xcb-xevie xcb-shape xcb-cursor xcb-res xcb-xprint xcb-xfixes) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "el_client" '("xelb-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb" '("xcb:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-bigreq" '("xcb:bigreq:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-composite" '("xcb:composite:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-cursor" '("xcb:cursor:-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-damage" '("xcb:damage:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-debug" '("xcb-debug:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-dpms" '("xcb:dpms:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-dri2" '("xcb:dri2:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-dri3" '("xcb:dri3:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-ewmh" '("xcb:ewmh:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-ge" '("xcb:ge:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-glx" '("xcb:glx:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-icccm" '("xcb:icccm:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-keysyms" '("xcb:keysyms:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-present" '("xcb:present:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-randr" '("xcb:randr:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-record" '("xcb:record:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-render" '("xcb:render:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-renderutil" '("xcb:renderutil:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-res" '("xcb:res:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-screensaver" '("xcb:screensaver:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-shape" '("xcb:shape:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-shm" '("xcb:shm:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-sync" '("xcb:sync:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-systemtray" '("xcb:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-types" '("xcb:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xc_misc" '("xcb:xc_misc:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xembed" '("xcb:xembed:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xevie" '("xcb:xevie:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xf86dri" '("xcb:xf86dri:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xf86vidmode" '("xcb:xf86vidmode:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xfixes" '("xcb:xfixes:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xim" '("xim:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xinerama" '("xcb:xinerama:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xinput" '("xcb:xinput:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xkb" '("xcb:xkb:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xlib" '("xlib:X"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xprint" '("xcb:xprint:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xproto" '("xcb:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xselinux" '("xcb:xselinux:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xtest" '("xcb:xtest:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xv" '("xcb:xv:"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xcb-xvmc" '("xcb:xvmc:"))) (provide 'xelb-autoloads)) "exwm" ((exwm-floating exwm-randr exwm-core exwm-layout exwm-xim exwm-systemtray exwm-autoloads exwm-input exwm exwm-cm exwm-config exwm-manage exwm-workspace) (autoload 'exwm-restart "exwm" "Restart EXWM." t nil) (autoload 'exwm-init "exwm" "Initialize EXWM.

(fn &optional FRAME)" t nil) (autoload 'exwm-exit "exwm" "Exit EXWM." t nil) (autoload 'exwm-enable "exwm" "Enable/Disable EXWM.

(fn &optional UNDO)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm" '("exwm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-cm" '("exwm-cm-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-config" '("exwm-config-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-core" '("exwm-"))) (autoload 'exwm-floating-toggle-floating "exwm-floating" "Toggle the current window between floating and non-floating states." t nil) (autoload 'exwm-floating-hide "exwm-floating" "Hide the current floating X window (which would show again when selected)." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-floating" '("exwm-floating-"))) (autoload 'exwm-input-set-key "exwm-input" "Set a global key binding.

The new key binding only takes effect in real time when this command is
called interactively, and is lost when this session ends unless it's
specifically saved in the Customize interface for `exwm-input-global-keys'.

In configuration you should customize or set `exwm-input-global-keys'
instead.

(fn KEY COMMAND)" t nil) (autoload 'exwm-input-grab-keyboard "exwm-input" "Switch to line-mode.

(fn &optional ID)" t nil) (autoload 'exwm-input-release-keyboard "exwm-input" "Switch to char-mode.

(fn &optional ID)" t nil) (autoload 'exwm-input-toggle-keyboard "exwm-input" "Toggle between 'line-mode' and 'char-mode'.

(fn &optional ID)" t nil) (autoload 'exwm-input-send-next-key "exwm-input" "Send next key to client window.

EXWM will prompt for the key to send.  This command can be prefixed to send
multiple keys.  If END-KEY is non-nil, stop sending keys if it's pressed.

(fn TIMES &optional END-KEY)" t nil) (autoload 'exwm-input-set-simulation-key "exwm-input" "Set a simulation key.

The simulation key takes effect in real time, but is lost when this session
ends unless it's specifically saved in the Customize interface for
`exwm-input-simulation-keys'.

(fn ORIGINAL-KEY SIMULATED-KEY)" t nil) (autoload 'exwm-input-send-simulation-key "exwm-input" "Fake a key event according to the last input key sequence.

(fn TIMES)" t nil) (autoload 'exwm-input-invoke-factory "exwm-input" "Make a command that invokes KEYS when called.

One use is to access the keymap bound to KEYS (as prefix keys) in char-mode.

(fn KEYS)" nil t) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-input" '("exwm-input-"))) (autoload 'exwm-layout-set-fullscreen "exwm-layout" "Make window ID fullscreen.

(fn &optional ID)" t nil) (autoload 'exwm-layout-unset-fullscreen "exwm-layout" "Restore window from fullscreen state.

(fn &optional ID)" t nil) (autoload 'exwm-layout-toggle-fullscreen "exwm-layout" "Toggle fullscreen mode.

(fn &optional ID)" t nil) (autoload 'exwm-layout-enlarge-window "exwm-layout" "Make the selected window DELTA pixels taller.

If no argument is given, make the selected window one pixel taller.  If the
optional argument HORIZONTAL is non-nil, make selected window DELTA pixels
wider.  If DELTA is negative, shrink selected window by -DELTA pixels.

Normal hints are checked and regarded if the selected window is displaying an
`exwm-mode' buffer.  However, this may violate the normal hints set on other X
windows.

(fn DELTA &optional HORIZONTAL)" t nil) (autoload 'exwm-layout-enlarge-window-horizontally "exwm-layout" "Make the selected window DELTA pixels wider.

See also `exwm-layout-enlarge-window'.

(fn DELTA)" t nil) (autoload 'exwm-layout-shrink-window "exwm-layout" "Make the selected window DELTA pixels lower.

See also `exwm-layout-enlarge-window'.

(fn DELTA)" t nil) (autoload 'exwm-layout-shrink-window-horizontally "exwm-layout" "Make the selected window DELTA pixels narrower.

See also `exwm-layout-enlarge-window'.

(fn DELTA)" t nil) (autoload 'exwm-layout-hide-mode-line "exwm-layout" "Hide mode-line." t nil) (autoload 'exwm-layout-show-mode-line "exwm-layout" "Show mode-line." t nil) (autoload 'exwm-layout-toggle-mode-line "exwm-layout" "Toggle the display of mode-line." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-layout" '("exwm-layout-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-manage" '("exwm-manage-"))) (autoload 'exwm-randr-refresh "exwm-randr" "Refresh workspaces according to the updated RandR info." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-randr" '("exwm-randr-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-systemtray" '("exwm-systemtray-" "xcb:systemtray:-ClientMessage"))) (autoload 'exwm-workspace--get-geometry "exwm-workspace" "Return the geometry of frame FRAME.

(fn FRAME)" nil nil) (autoload 'exwm-workspace--current-height "exwm-workspace" "Return the height of current workspace." nil nil) (autoload 'exwm-workspace--minibuffer-own-frame-p "exwm-workspace" "Reports whether the minibuffer is displayed in its own frame." nil nil) (autoload 'exwm-workspace-switch "exwm-workspace" "Switch to workspace INDEX (0-based).

Query for the index if not specified when called interactively.  Passing a
workspace frame as the first option or making use of the rest options are
for internal use only.

(fn FRAME-OR-INDEX &optional FORCE)" t nil) (autoload 'exwm-workspace-switch-create "exwm-workspace" "Switch to workspace INDEX or creating it first if it does not exist yet.

Passing a workspace frame as the first option is for internal use only.

(fn FRAME-OR-INDEX)" t nil) (autoload 'exwm-workspace-swap "exwm-workspace" "Interchange position of WORKSPACE1 with that of WORKSPACE2.

(fn WORKSPACE1 WORKSPACE2)" t nil) (autoload 'exwm-workspace-move "exwm-workspace" "Move WORKSPACE to the NTH position.

When called interactively, prompt for a workspace and move current one just
before it.

(fn WORKSPACE NTH)" t nil) (autoload 'exwm-workspace-add "exwm-workspace" "Add a workspace as the INDEX-th workspace, or the last one if INDEX is nil.

INDEX must not exceed the current number of workspaces.

(fn &optional INDEX)" t nil) (autoload 'exwm-workspace-delete "exwm-workspace" "Delete the workspace FRAME-OR-INDEX.

(fn &optional FRAME-OR-INDEX)" t nil) (autoload 'exwm-workspace-move-window "exwm-workspace" "Move window ID to workspace FRAME-OR-INDEX.

(fn FRAME-OR-INDEX &optional ID)" t nil) (autoload 'exwm-workspace-switch-to-buffer "exwm-workspace" "Make the current Emacs window display another buffer.

(fn BUFFER-OR-NAME)" t nil) (autoload 'exwm-workspace-attach-minibuffer "exwm-workspace" "Attach the minibuffer so that it always shows." t nil) (autoload 'exwm-workspace-detach-minibuffer "exwm-workspace" "Detach the minibuffer so that it automatically hides." t nil) (autoload 'exwm-workspace-toggle-minibuffer "exwm-workspace" "Attach the minibuffer if it's detached, or detach it if it's attached." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-workspace" '("exwm-workspace-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "exwm-xim" '("exwm-xim-"))) (provide 'exwm-autoloads)) "selectrum" ((selectrum selectrum-autoloads) (defvar selectrum-complete-in-buffer t "If non-nil, use Selectrum for `completion-in-region'.
This option needs to be set before activating `selectrum-mode'.") (custom-autoload 'selectrum-complete-in-buffer "selectrum" t) (autoload 'selectrum-select-from-history "selectrum" "Submit or insert candidate from minibuffer history.
To insert the history item into the previous session use the
binding for `selectrum-insert-current-candidate'. To submit the
history item and exit use `selectrum-select-current-candidate'." t nil) (autoload 'selectrum-completing-read "selectrum" "Read choice using Selectrum. Can be used as `completing-read-function'.
For PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT,
HIST, DEF, and INHERIT-INPUT-METHOD, see `completing-read'.

(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil) (autoload 'selectrum-completing-read-multiple "selectrum" "Read one or more choices using Selectrum.
Replaces `completing-read-multiple'. For PROMPT, TABLE,
PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD, see `completing-read-multiple'.

The option `selectrum-completing-read-multiple-show-help' can be
used to control insertion of additional usage information into
the prompt.

(fn PROMPT TABLE &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil) (autoload 'selectrum-completion-in-region "selectrum" "Complete in-buffer text using a list of candidates.
Can be used as `completion-in-region-function'. For START, END,
COLLECTION, and PREDICATE, see `completion-in-region'.

(fn START END COLLECTION PREDICATE)" nil nil) (autoload 'selectrum-read-buffer "selectrum" "Read buffer using Selectrum. Can be used as `read-buffer-function'.
Actually, as long as `selectrum-completing-read' is installed in
`completing-read-function', `read-buffer' already uses Selectrum.
Installing this function in `read-buffer-function' makes sure the
buffers are sorted in the default order (most to least recently
used) rather than in whatever order is defined by
`selectrum-preprocess-candidates-function', which is likely to be
less appropriate. It also allows you to view hidden buffers,
which is otherwise impossible due to tricky behavior of Emacs'
completion machinery. For PROMPT, DEF, REQUIRE-MATCH, and
PREDICATE, see `read-buffer'.

(fn PROMPT &optional DEF REQUIRE-MATCH PREDICATE)" nil nil) (autoload 'selectrum-read-file-name "selectrum" "Read file name using Selectrum. Can be used as `read-file-name-function'.
For PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and
PREDICATE, see `read-file-name'.

(fn PROMPT &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" nil nil) (autoload 'selectrum--fix-dired-read-dir-and-switches "selectrum" "Make \\[dired] do the \"right thing\" with its default candidate.
By default \\[dired] uses `read-file-name' internally, which
causes Selectrum to provide you with the first file inside the
working directory as the default candidate. However, it would
arguably be more semantically appropriate to use
`read-directory-name', and this is especially important for
Selectrum since this causes it to select the working directory
initially.

To test that this advice is working correctly, type \\[dired] and
accept the default candidate. You should have opened the working
directory in Dired, and not a filtered listing for the current
file.

This is an `:around' advice for `dired-read-dir-and-switches'.
FUNC and ARGS are standard as in any `:around' advice.

(fn FUNC &rest ARGS)" nil nil) (autoload 'selectrum-read-library-name "selectrum" "Read and return a library name.
Similar to `read-library-name' except it handles `load-path'
shadows correctly." nil nil) (autoload 'selectrum--fix-minibuffer-message "selectrum" "Ensure the cursor stays at the front of the minibuffer message.
This advice adjusts where the cursor gets placed for the overlay
of `minibuffer-message' and ensures the overlay gets displayed at
the right place without blocking the display of candidates.

To test that this advice is working correctly, type \\[find-file]
twice in a row with `enable-recursive-minibuffers' set to nil.
The overlay indicating that recursive minibuffers are not allowed
should appear right after the user input area, not at the end of
the candidate list and the cursor should stay at the front.

This is an `:around' advice for `minibuffer-message'. FUNC and
ARGS are standard as in all `:around' advice.

(fn FUNC &rest ARGS)" nil nil) (define-minor-mode selectrum-mode "Minor mode to use Selectrum for `completing-read'." :global t (if selectrum-mode (progn (selectrum-mode -1) (setq selectrum-mode t) (setq selectrum--old-completing-read-function (default-value 'completing-read-function)) (setq-default completing-read-function #'selectrum-completing-read) (setq selectrum--old-read-buffer-function (default-value 'read-buffer-function)) (setq-default read-buffer-function #'selectrum-read-buffer) (setq selectrum--old-read-file-name-function (default-value 'read-file-name-function)) (setq-default read-file-name-function #'selectrum-read-file-name) (setq selectrum--old-completion-in-region-function (default-value 'completion-in-region-function)) (when selectrum-complete-in-buffer (setq-default completion-in-region-function #'selectrum-completion-in-region)) (advice-add #'completing-read-multiple :override #'selectrum-completing-read-multiple) (advice-add 'dired-read-dir-and-switches :around #'selectrum--fix-dired-read-dir-and-switches) (advice-add 'read-library-name :override #'selectrum-read-library-name) (advice-add #'minibuffer-message :around #'selectrum--fix-minibuffer-message) (define-key minibuffer-local-map [remap previous-matching-history-element] 'selectrum-select-from-history)) (when (equal (default-value 'completing-read-function) #'selectrum-completing-read) (setq-default completing-read-function selectrum--old-completing-read-function)) (when (equal (default-value 'read-buffer-function) #'selectrum-read-buffer) (setq-default read-buffer-function selectrum--old-read-buffer-function)) (when (equal (default-value 'read-file-name-function) #'selectrum-read-file-name) (setq-default read-file-name-function selectrum--old-read-file-name-function)) (when (equal (default-value 'completion-in-region-function) #'selectrum-completion-in-region) (setq-default completion-in-region-function selectrum--old-completion-in-region-function)) (advice-remove #'completing-read-multiple #'selectrum-completing-read-multiple) (advice-remove 'dired-read-dir-and-switches #'selectrum--fix-dired-read-dir-and-switches) (advice-remove 'read-library-name #'selectrum-read-library-name) (advice-remove #'minibuffer-message #'selectrum--fix-minibuffer-message) (when (eq (lookup-key minibuffer-local-map [remap previous-matching-history-element]) #'selectrum-select-from-history) (define-key minibuffer-local-map [remap previous-matching-history-element] nil)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectrum" '("selectrum-"))) (provide 'selectrum-autoloads)) "prescient" ((prescient prescient-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "prescient" '("prescient-"))) (provide 'prescient-autoloads)) "selectrum-prescient" ((selectrum-prescient-autoloads selectrum-prescient) (defvar selectrum-prescient-mode nil "Non-nil if Selectrum-Prescient mode is enabled.
See the `selectrum-prescient-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `selectrum-prescient-mode'.") (custom-autoload 'selectrum-prescient-mode "selectrum-prescient" nil) (autoload 'selectrum-prescient-mode "selectrum-prescient" "Minor mode to use prescient.el in Selectrum menus.

If called interactively, enable Selectrum-Prescient mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectrum-prescient" '("selectrum-prescient-"))) (provide 'selectrum-prescient-autoloads)) "consult" ((consult-org consult-xref consult-autoloads consult-register consult-selectrum consult-compile consult-icomplete consult-flymake consult-vertico consult consult-imenu) (autoload 'consult-completion-in-region "consult" "Use minibuffer completion as the UI for `completion-at-point'.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

The function can be configured via `consult-customize'.

    (consult-customize consult-completion-in-region
                       :completion-styles (basic)
                       :cycle-threshold 3)

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer

(fn START END COLLECTION &optional PREDICATE)" nil nil) (autoload 'consult-completing-read-multiple "consult" "Enhanced replacement for `completing-read-multiple'.
See `completing-read-multiple' for the documentation of the arguments.

(fn PROMPT TABLE &optional PRED REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil) (autoload 'consult-multi-occur "consult" "Improved version of `multi-occur' based on `completing-read-multiple'.

See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES.

(fn BUFS REGEXP &optional NLINES)" t nil) (autoload 'consult-outline "consult" "Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate preview.
The symbol at point is added to the future history." t nil) (autoload 'consult-mark "consult" "Jump to a marker in MARKERS list (defaults to buffer-local `mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t nil) (autoload 'consult-global-mark "consult" "Jump to a marker in MARKERS list (defaults to `global-mark-ring').

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history.

(fn &optional MARKERS)" t nil) (autoload 'consult-line "consult" "Search for a matching line.

Depending on the setting `consult-line-point-placement' the command jumps to
the beginning or the end of the first match on the line or the line beginning.
The default candidate is the non-empty line next to point. This command obeys
narrowing. Optional INITIAL input can be provided. The search starting point is
changed if the START prefix argument is set. The symbol at point and the last
`isearch-string' is added to the future history.

(fn &optional INITIAL START)" t nil) (autoload 'consult-line-multi "consult" "Search for a matching line in multiple buffers.

By default search across all project buffers. If the prefix argument QUERY is
non-nil, all buffers are searched. Optional INITIAL input can be provided. See
`consult-line' for more information. In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn QUERY &optional INITIAL)" t nil) (autoload 'consult-keep-lines "consult" "Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. When
called from elisp, the filtering is performed by a FILTER function. This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn &optional FILTER INITIAL)" t nil) (autoload 'consult-focus-lines "consult" "Hide or show lines using overlays.

The selected lines are shown and the other lines hidden. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. With
optional prefix argument SHOW reveal the hidden lines. Alternatively the
command can be restarted to reveal the lines. When called from elisp, the
filtering is performed by a FILTER function. This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

(fn &optional SHOW FILTER INITIAL)" t nil) (autoload 'consult-goto-line "consult" "Read line number and jump to the line with preview.

Jump directly if a line number is given as prefix ARG. The command respects
narrowing and the settings `consult-goto-line-numbers' and
`consult-line-numbers-widen'.

(fn &optional ARG)" t nil) (autoload 'consult-recent-file "consult" "Find recent file using `completing-read'." t nil) (autoload 'consult-file-externally "consult" "Open FILE externally using the default application of the system.

(fn FILE)" t nil) (autoload 'consult-mode-command "consult" "Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

(fn &rest MODES)" t nil) (autoload 'consult-yank-from-kill-ring "consult" "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string.

(fn STRING &optional ARG)" t nil) (autoload 'consult-yank-pop "consult" "If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string.

(fn &optional ARG)" t nil) (autoload 'consult-yank-replace "consult" "Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

There exists no equivalent of this command in Emacs 28.

(fn STRING)" t nil) (autoload 'consult-bookmark "consult" "If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration.

(fn NAME)" t nil) (autoload 'consult-apropos "consult" "Select pattern and call `apropos'.

The default value of the completion is the symbol at point. As a better
alternative, you can run `embark-export' from commands like `M-x' and
`describe-symbol'." t nil) (autoload 'consult-complex-command "consult" "Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t nil) (autoload 'consult-history "consult" "Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable
as argument.

(fn &optional HISTORY)" t nil) (autoload 'consult-isearch-history "consult" "Read a search string with completion from the Isearch history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t nil) (autoload 'consult-minor-mode-menu "consult" "Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t nil) (autoload 'consult-theme "consult" "Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

(fn THEME)" t nil) (autoload 'consult-buffer "consult" "Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as virtual
buffers. Buffers are previewed. Furthermore narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding keys. In
order to determine the project-specific files and buffers, the
`consult-project-root-function' is used. See `consult-buffer-sources' and
`consult--multi' for the configuration of the virtual buffer sources." t nil) (autoload 'consult-buffer-other-window "consult" "Variant of `consult-buffer' which opens in other window." t nil) (autoload 'consult-buffer-other-frame "consult" "Variant of `consult-buffer' which opens in other frame." t nil) (autoload 'consult-kmacro "consult" "Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

(fn ARG)" t nil) (autoload 'consult-grep "consult" "Search for regexp with grep in DIR with INITIAL input.

The input string is split, the first part of the string (grep input) is
passed to the asynchronous grep process and the second part of the string is
passed to the completion-style filtering.

The input string is split at a punctuation character, which is given as the
first character of the input string. The format is similar to Perl-style
regular expressions, e.g., /regexp/. Furthermore command line options can be
passed to grep, specified behind --. The overall prompt input has the form
`#async-input -- grep-opts#filter-string'.

Note that the grep input string is transformed from Emacs regular expressions
to Posix regular expressions. Always enter Emacs regular expressions at the
prompt. `consult-grep' behaves like builtin Emacs search commands, e.g.,
Isearch, which take Emacs regular expressions. Furthermore the asynchronous
input split into words, each word must match separately and in any order. See
`consult--regexp-compiler' for the inner workings. In order to disable
transformations of the grep input, adjust `consult--regexp-compiler'
accordingly.

Here we give a few example inputs:

#alpha beta         : Search for alpha and beta in any order.
#alpha.*beta        : Search for alpha before beta.
#\\(alpha\\|beta\\) : Search for alpha or beta (Note Emacs syntax!)
#word -- -C3        : Search for word, include 3 lines as context
#first#second       : Search for first, quick filter for second.

The symbol at point is added to the future history. If `consult-grep'
is called interactively with a prefix argument, the user can specify
the directory to search in. By default the project directory is used
if `consult-project-root-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-git-grep "consult" "Search for regexp with grep in DIR with INITIAL input.

See `consult-grep' for more details.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-ripgrep "consult" "Search for regexp with rg in DIR with INITIAL input.

See `consult-grep' for more details.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-find "consult" "Search for regexp with find in DIR with INITIAL input.

The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

(fn &optional DIR INITIAL)" t nil) (autoload 'consult-locate "consult" "Search for regexp with locate with INITIAL input.

The locate process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

(fn &optional INITIAL)" t nil) (autoload 'consult-man "consult" "Search for regexp with man with INITIAL input.

The man process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

(fn &optional INITIAL)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult" '("consult-"))) (autoload 'consult-compile-error "consult-compile" "Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-compile" '("consult-compile--"))) (autoload 'consult-flymake "consult-flymake" "Jump to Flymake diagnostic." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-flymake" '("consult-flymake--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-icomplete" '("consult-icomplete--refresh"))) (autoload 'consult-imenu "consult-imenu" "Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing. See the variable
`consult-imenu-config', which configures the narrowing.
The symbol at point is added to the future history.

See also `consult-imenu-multi'." t nil) (autoload 'consult-imenu-multi "consult-imenu" "Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-root-function' is used. Only the buffers with the
same major mode as the current buffer are used. See also
`consult-imenu' for more details. In order to search a subset of buffers,
QUERY can be set to a plist according to `consult--buffer-query'.

(fn &optional QUERY)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-imenu" '("consult-imenu-"))) (autoload 'consult-org-heading "consult-org" "Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered.

(fn &optional MATCH SCOPE)" t nil) (autoload 'consult-org-agenda "consult-org" "Jump to an Org agenda heading.

By default, all agenda entries are offered. MATCH is as in
`org-map-entries' and can used to refine this.

(fn &optional MATCH)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-org" '("consult-org--"))) (autoload 'consult-register-window "consult-register" "Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

(fn BUFFER &optional SHOW-EMPTY)" nil nil) (autoload 'consult-register-format "consult-register" "Enhanced preview of register REG.

This function can be used as `register-preview-function'.

(fn REG)" nil nil) (autoload 'consult-register "consult-register" "Load register and either jump to location or insert the stored text.

This command is useful to search the register contents. For quick access to
registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in built-in
register access functions. The command supports narrowing, see
`consult-register-narrow'. Marker positions are previewed. See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

(fn &optional ARG)" t nil) (autoload 'consult-register-load "consult-register" "Do what I mean with a REG.

For a window configuration, restore it. For a number or text, insert it. For a
location, jump to it. See `jump-to-register' and `insert-register' for the
meaning of prefix ARG.

(fn REG &optional ARG)" t nil) (autoload 'consult-register-store "consult-register" "Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally deleting
the region when a prefix ARG is given. With a numeric prefix ARG, store/add the
number. Otherwise store point, frameset, window or kmacro.

(fn ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-register" '("consult-register-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-selectrum" '("consult-selectrum--"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-vertico" '("consult-vertico--"))) (autoload 'consult-xref "consult-xref" "Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

(fn FETCHER &optional ALIST)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-xref" '("consult-xref--"))) (provide 'consult-autoloads)) "marginalia" ((marginalia-autoloads marginalia) (defvar marginalia-mode nil "Non-nil if Marginalia mode is enabled.
See the `marginalia-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `marginalia-mode'.") (custom-autoload 'marginalia-mode "marginalia" nil) (autoload 'marginalia-mode "marginalia" "Annotate completion candidates with richer information.

If called interactively, enable Marginalia mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (autoload 'marginalia-cycle "marginalia" "Cycle between annotators in `marginalia-annotator-registry'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "marginalia" '("marginalia-"))) (provide 'marginalia-autoloads)) "vertico" ((vertico vertico-autoloads) (defvar vertico-mode nil "Non-nil if Vertico mode is enabled.
See the `vertico-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `vertico-mode'.") (custom-autoload 'vertico-mode "vertico" nil) (autoload 'vertico-mode "vertico" "VERTical Interactive COmpletion.

If called interactively, enable Vertico mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vertico" '("vertico-"))) (provide 'vertico-autoloads)) "orderless" ((orderless-autoloads orderless) (autoload 'orderless-filter "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.

(fn STRING TABLE &optional PRED)" nil nil) (autoload 'orderless-all-completions "orderless" "Split STRING into components and find entries TABLE matching all.
The predicate PRED is used to constrain the entries in TABLE.  The
matching portions of each candidate are highlighted.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)" nil nil) (autoload 'orderless-try-completion "orderless" "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style.

(fn STRING TABLE PRED POINT)" nil nil) (add-to-list 'completion-styles-alist '(orderless orderless-try-completion orderless-all-completions "Completion of multiple components, in any order.")) (autoload 'orderless-ivy-re-builder "orderless" "Convert STR into regexps for use with ivy.
This function is for integration of orderless with ivy, use it as
a value in `ivy-re-builders-alist'.

(fn STR)" nil nil) (with-eval-after-load 'ivy (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orderless" '("orderless-"))) (provide 'orderless-autoloads)) "corfu" ((corfu corfu-autoloads) (autoload 'corfu-mode "corfu" "Completion Overlay Region FUnction

If called interactively, enable Corfu mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'corfu-global-mode 'globalized-minor-mode t) (defvar corfu-global-mode nil "Non-nil if Corfu-Global mode is enabled.
See the `corfu-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `corfu-global-mode'.") (custom-autoload 'corfu-global-mode "corfu" nil) (autoload 'corfu-global-mode "corfu" "Toggle Corfu mode in all buffers.
With prefix ARG, enable Corfu-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Corfu mode is enabled in all buffers where
`corfu--on' would do it.
See `corfu-mode' for more information on Corfu mode.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "corfu" '("corfu-"))) (provide 'corfu-autoloads)) "ligature" ((ligature ligature-autoloads) (autoload 'ligature-set-ligatures "ligature" "Replace LIGATURES in MODES.

Converts a list of LIGATURES, where each element is either a cons
cell of `(STR-CHAR . REGEXP)' or a string to ligate, for all
modes in MODES.  As there is no easy way of computing which
ligatures were already defined, this function will replace any
existing ligature definitions in `ligature-composition-table'
with LIGATURES for MODES.


Some ligatures are variable-length, such as arrows and borders,
and need a regular expression to accurately represent the range
of characters needed to ligate them.  In that case, you must use a
cons cell of `(STR-CHAR . REGEXP)' where `STR-CHR' is the first
character in the ligature and `REGEXP' is a regular expression
that matches the _rest_ of the ligature range.

For examples, see the commentary in `ligature.el'.

(fn MODES LIGATURES)" nil nil) (autoload 'ligature-generate-ligatures "ligature" "Ligate the current buffer using its major mode to determine ligature sets.

The ligature generator traverses `ligature-composition-table' and
applies every ligature definition from every mode that matches
either `t' (indicating that a ligature mapping always applies);
or a major mode or list of major mode symbols that are
`derived-mode-p' of the current buffer's major mode.

The changes are then made buffer-local." t nil) (autoload 'ligature-mode "ligature" "Enables typographic ligatures

If called interactively, enable Ligature mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ligature" '("global-ligature-mode" "ligature-" "turn-on-ligature-mode"))) (provide 'ligature-autoloads)) "pcache" ((pcache pcache-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pcache" '("*pcache-repositor" "pcache-"))) (provide 'pcache-autoloads)) "list-utils" ((list-utils-autoloads list-utils) (let ((loads (get 'list-utils 'custom-loads))) (if (member '"list-utils" loads) nil (put 'list-utils 'custom-loads (cons '"list-utils" loads)))) (require 'cl) (cl-defstruct tconc head tail) (autoload 'tconc-list "list-utils" "Efficiently append LIST to TC.

TC is a data structure created by `make-tconc'.

(fn TC LIST)" nil nil) (autoload 'tconc "list-utils" "Efficiently append ARGS to TC.

TC is a data structure created by `make-tconc'

Without ARGS, return the list held by TC.

(fn TC &rest ARGS)" nil nil) (autoload 'list-utils-cons-cell-p "list-utils" "Return non-nil if CELL holds a cons cell rather than a proper list.

A proper list is defined as a series of cons cells in which the
cdr slot of each cons holds a pointer to the next element of the
list, and the cdr slot in the final cons holds nil.

A plain cons cell, for the purpose of this function, is a single
cons in which the cdr holds data rather than a pointer to the
next cons cell, eg

    '(1 . 2)

In addition, a list which is not nil-terminated is not a proper
list and will be recognized by this function as a cons cell.
Such a list is printed using dot notation for the last two
elements, eg

    '(1 2 3 4 . 5)

Such improper lists are produced by `list*'.

(fn CELL)" nil nil) (autoload 'list-utils-make-proper-copy "list-utils" "Copy a cons cell or improper LIST into a proper list.

If optional TREE is non-nil, traverse LIST, making proper
copies of any improper lists contained within.

Optional RECUR-INTERNAL is for internal use only.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

(fn LIST &optional TREE RECUR-INTERNAL)" nil nil) (autoload 'list-utils-make-proper-inplace "list-utils" "Make a cons cell or improper LIST into a proper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

If optional TREE is non-nil, traverse LIST, making any
improper lists contained within into proper lists.

Optional RECUR-INTERNAL is for internal use only.

Modifies LIST and returns the modified value.

(fn LIST &optional TREE RECUR-INTERNAL)" nil nil) (autoload 'list-utils-make-improper-copy "list-utils" "Copy a proper LIST into an improper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

If optional TREE is non-nil, traverse LIST, making proper
copies of any improper lists contained within.

Optional RECUR-INTERNAL is for internal use only.

(fn LIST &optional TREE RECUR-INTERNAL)" nil nil) (autoload 'list-utils-make-improper-inplace "list-utils" "Make proper LIST into an improper list.

Improper lists consist of proper lists consed to a final
element, and are produced by `list*'.

If optional TREE is non-nil, traverse LIST, making any
proper lists contained within into improper lists.

Optional RECUR-INTERNAL is for internal use only.

Modifies LIST and returns the modified value.

(fn LIST &optional TREE RECUR-INTERNAL)" nil nil) (autoload 'list-utils-linear-subseq "list-utils" "Return the linear elements from a partially cyclic LIST.

If there is no cycle in LIST, return LIST.  If all elements of
LIST are included in a cycle, return nil.

As an optimization, CYCLE-LENGTH may be specified if the length
of the cyclic portion is already known.  Otherwise it will be
calculated from LIST.

(fn LIST &optional CYCLE-LENGTH)" nil nil) (autoload 'list-utils-cyclic-subseq "list-utils" "Return any cyclic elements from LIST as a circular list.

The first element of the cyclic structure is not guaranteed to be
first element of the return value unless FROM-START is non-nil.

To linearize the return value, use `list-utils-make-linear-inplace'.

If there is no cycle in LIST, return nil.

(fn LIST &optional FROM-START)" nil nil) (autoload 'list-utils-cyclic-length "list-utils" "Return the number of cyclic elements in LIST.

If some portion of LIST is linear, only the cyclic
elements will be counted.

If LIST is completely linear, return 0.

(fn LIST)" nil nil) (autoload 'list-utils-cyclic-p "list-utils" "Return non-nil if LIST contains any cyclic structures.

If optional PERFECT is set, only return non-nil if LIST is a
perfect non-branching cycle in which the last element points
to the first.

(fn LIST &optional PERFECT)" nil nil) (autoload 'list-utils-linear-p "list-utils" "Return non-nil if LIST is linear (no cyclic structure).

(fn LIST)" nil nil) (defalias 'list-utils-improper-p 'list-utils-cons-cell-p) (autoload 'list-utils-safe-length "list-utils" "Return the number of elements in LIST.

LIST may be linear or cyclic.

If LIST is not really a list, returns 0.

If LIST is an improper list, return the number of proper list
elements, like `safe-length'.

(fn LIST)" nil nil) (autoload 'list-utils-flat-length "list-utils" "Count simple elements from the beginning of LIST.

Stop counting when a cons is reached.  nil is not a cons,
and is considered to be a \"simple\" element.

If the car of LIST is a cons, return 0.

(fn LIST)" nil nil) (autoload 'list-utils-make-linear-copy "list-utils" "Return a linearized copy of LIST, which may be cyclic.

If optional TREE is non-nil, traverse LIST, substituting
linearized copies of any cyclic lists contained within.

(fn LIST &optional TREE)" nil nil) (autoload 'list-utils-make-linear-inplace "list-utils" "Linearize LIST, which may be cyclic.

Modifies LIST and returns the modified value.

If optional TREE is non-nil, traverse LIST, linearizing any
cyclic lists contained within.

(fn LIST &optional TREE)" nil nil) (autoload 'list-utils-safe-equal "list-utils" "Compare LIST-1 and LIST-2, which may be cyclic lists.

LIST-1 and LIST-2 may also contain cyclic lists, which are
each traversed and compared.  This function will not infloop
when cyclic lists are encountered.

Non-nil is returned only if the leaves of LIST-1 and LIST-2 are
`equal' and the structure is identical.

Optional TEST specifies a test, defaulting to `equal'.

If LIST-1 and LIST-2 are not actually lists, they are still
compared according to TEST.

(fn LIST-1 LIST-2 &optional TEST)" nil nil) (autoload 'list-utils-depth "list-utils" "Find the depth of LIST, which may contain other lists.

If LIST is not a list or is an empty list, returns a depth
of 0.

If LIST is a cons cell or a list which does not contain other
lists, returns a depth of 1.

(fn LIST)" nil nil) (autoload 'list-utils-flatten "list-utils" "Return a flattened copy of LIST, which may contain other lists.

This function flattens cons cells as lists, and
flattens circular list structures.

(fn LIST)" nil nil) (autoload 'list-utils-insert-before "list-utils" "Look in LIST for ELEMENT and insert NEW-ELEMENT before it.

Optional TEST sets the test used for a matching element, and
defaults to `equal'.

LIST is modified and the new value is returned.

(fn LIST ELEMENT NEW-ELEMENT &optional TEST)" nil nil) (autoload 'list-utils-insert-after "list-utils" "Look in LIST for ELEMENT and insert NEW-ELEMENT after it.

Optional TEST sets the test used for a matching element, and
defaults to `equal'.

LIST is modified and the new value is returned.

(fn LIST ELEMENT NEW-ELEMENT &optional TEST)" nil nil) (autoload 'list-utils-insert-before-pos "list-utils" "Look in LIST for position POS, and insert NEW-ELEMENT before.

POS is zero-indexed.

LIST is modified and the new value is returned.

(fn LIST POS NEW-ELEMENT)" nil nil) (autoload 'list-utils-insert-after-pos "list-utils" "Look in LIST for position POS, and insert NEW-ELEMENT after.

LIST is modified and the new value is returned.

(fn LIST POS NEW-ELEMENT)" nil nil) (autoload 'list-utils-and "list-utils" "Return the elements of LIST1 which are present in LIST2.

This is similar to `cl-intersection' (or `intersection') from
the cl library, except that `list-utils-and' preserves order,
does not uniquify the results, and exhibits more predictable
performance for large lists.

Order will follow LIST1.  Duplicates may be present in the result
as in LIST1.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed.  When FLIP is set, LIST2 will be the guide for the
order of the result, and will determine whether duplicates may
be returned.  Since this function preserves duplicates, setting
FLIP can change the number of elements in the result.

Performance: `list-utils-and' and friends use a general-purpose
hashing approach.  `intersection' and friends use pure iteration.
Iteration can be much faster in a few special cases, especially
when the number of elements is small.  In other scenarios,
iteration can be much slower.  Hashing has no worst-case
performance scenario, although it uses much more memory.  For
heavy-duty list operations, performance may be improved by
`let'ing `gc-cons-threshold' to a high value around sections that
make frequent use of this function.

(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil) (autoload 'list-utils-not "list-utils" "Return the elements of LIST1 which are not present in LIST2.

This is similar to `cl-set-difference' (or `set-difference') from
the cl library, except that `list-utils-not' preserves order and
exhibits more predictable performance for large lists.  Order will
follow LIST1.  Duplicates may be present as in LIST1.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed, returning elements of LIST2 which are not present in
LIST1.  When FLIP is set, LIST2 will be the guide for the order
of the result, and will determine whether duplicates may be
returned.

Performance: see notes under `list-utils-and'.

(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil) (autoload 'list-utils-xor "list-utils" "Return elements which are only present in either LIST1 or LIST2.

This is similar to `cl-set-exclusive-or' (or `set-exclusive-or')
from the cl library, except that `list-utils-xor' preserves order,
and exhibits more predictable performance for large lists.  Order
will follow LIST1, then LIST2.  Duplicates may be present as in
LIST1 or LIST2.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
the list to be hashed (LIST2 unless FLIP is set).

When optional FLIP is set, the sense of the comparison is
reversed, causing order and duplicates to follow LIST2, then
LIST1.

Performance: see notes under `list-utils-and'.

(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil) (autoload 'list-utils-uniq "list-utils" "Return a uniquified copy of LIST, preserving order.

This is similar to `cl-remove-duplicates' (or `remove-duplicates')
from the cl library, except that `list-utils-uniq' preserves order,
and exhibits more predictable performance for large lists.  Order
will follow LIST.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)" nil nil) (autoload 'list-utils-dupes "list-utils" "Return only duplicated elements from LIST, preserving order.

Duplicated elements may still exist in the result: this function
removes singlets.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)" nil nil) (autoload 'list-utils-singlets "list-utils" "Return only singlet elements from LIST, preserving order.

Duplicated elements may not exist in the result.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)" nil nil) (autoload 'list-utils-partition-dupes "list-utils" "Partition LIST into duplicates and singlets, preserving order.

The return value is an alist with two keys: 'dupes and 'singlets.
The two values of the alist are lists which, if combined, comprise
a complete copy of the elements of LIST.

Duplicated elements may still exist in the 'dupes partition.

TEST is an optional comparison function in the form of a
hash-table-test.  The default is `equal'.  Other valid values
include `eq' (built-in), `eql' (built-in), `list-utils-htt-='
(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).
See `define-hash-table-test' to define your own tests.

HINT is an optional micro-optimization, predicting the size of
LIST.

Performance: see notes under `list-utils-and'.

(fn LIST &optional TEST HINT)" nil nil) (autoload 'list-utils-alist-or-flat-length "list-utils" "Count simple or cons-cell elements from the beginning of LIST.

Stop counting when a proper list of non-zero length is reached.

If the car of LIST is a list, return 0.

(fn LIST)" nil nil) (autoload 'list-utils-alist-flatten "list-utils" "Flatten LIST, which may contain other lists.  Do not flatten cons cells.

It is not guaranteed that the result contains *only* cons cells.
The result could contain other data types present in LIST.

This function simply avoids flattening single conses or improper
lists where the last two elements would be expressed as a dotted
pair.

(fn LIST)" nil nil) (autoload 'list-utils-plist-reverse "list-utils" "Return reversed copy of property-list PLIST, maintaining pair associations.

(fn PLIST)" nil nil) (autoload 'list-utils-plist-del "list-utils" "Delete from PLIST the property PROP and its associated value.

When PROP is not present in PLIST, there is no effect.

The new plist is returned; use `(setq x (list-utils-plist-del x prop))'
to be sure to use the new value.

This functionality overlaps with the undocumented `cl-do-remf'.

(fn PLIST PROP)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "list-utils" '("list-utils-htt-"))) (provide 'list-utils-autoloads)) "persistent-soft" ((persistent-soft-autoloads persistent-soft) (let ((loads (get 'persistent-soft 'custom-loads))) (if (member '"persistent-soft" loads) nil (put 'persistent-soft 'custom-loads (cons '"persistent-soft" loads)))) (autoload 'persistent-soft-location-readable "persistent-soft" "Return non-nil if LOCATION is a readable persistent-soft data store.

(fn LOCATION)" nil nil) (autoload 'persistent-soft-location-destroy "persistent-soft" "Destroy LOCATION (a persistent-soft data store).

Returns non-nil on confirmed success.

(fn LOCATION)" nil nil) (autoload 'persistent-soft-exists-p "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Returns nil on failure, without throwing an error.

(fn SYMBOL LOCATION)" nil nil) (autoload 'persistent-soft-fetch "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Returns nil on failure, without throwing an error.

(fn SYMBOL LOCATION)" nil nil) (autoload 'persistent-soft-flush "persistent-soft" "Flush data for the LOCATION data store to disk.

(fn LOCATION)" nil nil) (autoload 'persistent-soft-store "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Optional EXPIRATION sets an expiry time in seconds.

Returns a true value if storage was successful.  Returns nil
on failure, without throwing an error.

(fn SYMBOL VALUE LOCATION &optional EXPIRATION)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "persistent-soft" '("persistent-soft-"))) (provide 'persistent-soft-autoloads)) "font-utils" ((font-utils font-utils-autoloads) (let ((loads (get 'font-utils 'custom-loads))) (if (member '"font-utils" loads) nil (put 'font-utils 'custom-loads (cons '"font-utils" loads)))) (autoload 'font-utils-client-hostname "font-utils" "Guess the client hostname, respecting $SSH_CONNECTION." nil nil) (autoload 'font-utils-name-from-xlfd "font-utils" "Return the font-family name from XLFD, a string.

This function accounts for the fact that the XLFD
delimiter, \"-\", is a legal character within fields.

(fn XLFD)" nil nil) (autoload 'font-utils-parse-name "font-utils" "Parse FONT-NAME which may contain fontconfig-style specifications.

Returns two-element list.  The car is the font family name as a string.
The cadr is the specifications as a normalized and sorted list.

(fn FONT-NAME)" nil nil) (autoload 'font-utils-normalize-name "font-utils" "Normalize FONT-NAME which may contain fontconfig-style specifications.

(fn FONT-NAME)" nil nil) (autoload 'font-utils-lenient-name-equal "font-utils" "Leniently match two strings, FONT-NAME-A and FONT-NAME-B.

(fn FONT-NAME-A FONT-NAME-B)" nil nil) (autoload 'font-utils-is-qualified-variant "font-utils" "Whether FONT-NAME-1 and FONT-NAME-2 are different variants of the same font.

Qualifications are fontconfig-style specifications added to a
font name, such as \":width=condensed\".

To return t, the font families must be identical, and the
qualifications must differ.  If FONT-NAME-1 and FONT-NAME-2 are
identical, returns nil.

(fn FONT-NAME-1 FONT-NAME-2)" nil nil) (autoload 'font-utils-list-names "font-utils" "Return a list of all font names on the current system." nil nil) (autoload 'font-utils-read-name "font-utils" "Read a font name using `completing-read'.

Underscores are removed from the return value.

Uses `ido-completing-read' if optional IDO is set.

(fn &optional IDO)" nil nil) (autoload 'font-utils-exists-p "font-utils" "Test whether FONT-NAME (a string or font object) exists.

FONT-NAME is a string, typically in Fontconfig font-name format.
A font-spec, font-vector, or font-object are accepted, though
the behavior for the latter two is not well defined.

Returns a matching font vector.

When POINT-SIZE is set, check for a specific font size.  Size may
also be given at the end of a string FONT-NAME, eg \"Monaco-12\".

When optional STRICT is given, FONT-NAME must will not be
leniently modified before passing to `font-info'.

Optional SCOPE is a list of font names, within which FONT-NAME
must (leniently) match.

(fn FONT-NAME &optional POINT-SIZE STRICT SCOPE)" nil nil) (autoload 'font-utils-first-existing-font "font-utils" "Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked.

If NO-NORMALIZE is given, the return value is exactly as the
member of FONT-NAMES.  Otherwise, the family name is extracted
from the XLFD returned by `font-info'.

(fn FONT-NAMES &optional NO-NORMALIZE)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "font-utils" '("font-" "persistent-softest-"))) (provide 'font-utils-autoloads)) "ucs-utils" ((ucs-utils-6\.0-delta ucs-utils ucs-utils-autoloads) (let ((loads (get 'ucs-utils 'custom-loads))) (if (member '"ucs-utils" loads) nil (put 'ucs-utils 'custom-loads (cons '"ucs-utils" loads)))) (autoload 'ucs-utils-pretty-name "ucs-utils" "Return a prettified UCS name for CHAR.

Based on `get-char-code-property'.  The result has been
title-cased for readability, and will not match into the
`ucs-utils-names' alist until it has been upcased.
`ucs-utils-char' can be used on the title-cased name.

Returns a hexified string if no name is found.  If NO-HEX is
non-nil, then a nil value will be returned when no name is
found.

(fn CHAR &optional NO-HEX)" nil nil) (autoload 'ucs-utils-all-prettified-names "ucs-utils" "All prettified UCS names, cached in list `ucs-utils-all-prettified-names'.

When optional PROGRESS is given, show progress when generating
cache.

When optional REGENERATE is given, re-generate cache.

(fn &optional PROGRESS REGENERATE)" nil nil) (autoload 'ucs-utils-char "ucs-utils" "Return the character corresponding to NAME, a UCS name.

NAME is matched leniently by `ucs-utils--lookup'.

Returns FALLBACK if NAME does not exist or is not displayable
according to TEST.  FALLBACK may be either a UCS name or
character, or one of the special symbols described in the next
paragraph.

If FALLBACK is nil or 'drop, returns nil on failure.  If FALLBACK
is 'error, throws an error on failure.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

When NAME is a character, it passes through unchanged, unless
TEST is set, in which case it must pass TEST.

(fn NAME &optional FALLBACK TEST)" nil nil) (autoload 'ucs-utils-first-existing-char "ucs-utils" "Return the first existing character from SEQUENCE of character names.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

(fn SEQUENCE &optional TEST)" nil nil) (autoload 'ucs-utils-vector "ucs-utils" "Return a vector corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string or character, it will be coerced
to a list of length 1.  Each name in SEQUENCE is matched
leniently by `ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil, characters which do not exist or are
undisplayable will be given as nils in the return value.  When
FALLBACK is 'drop, such characters will be silently dropped from
the return value.  When FALLBACK is 'error, such characters cause
an error to be thrown.

To allow fallbacks of arbitrary length, give FALLBACK as a vector-
of-vectors, with outer length equal to the length of sequence.  The
inner vectors may contain a sequence of characters, a literal
string, or nil.  Eg

   (ucs-utils-vector '(\"Middle Dot\" \"Ampersand\" \"Horizontal Ellipsis\")
                     '[?.           [?a ?n ?d]  [\"...\"]              ])

or

   (ucs-utils-vector \"Horizontal Ellipsis\" '[[\"...\"]])

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

If NO-FLATTEN is non-nil, then a vector-of-vectors may be returned
if multi-character fallbacks were used as in the example above.

(fn SEQUENCE &optional FALLBACK TEST NO-FLATTEN)" nil nil) (autoload 'ucs-utils-string "ucs-utils" "Return a string corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil or 'drop, characters which do not exist or
are undisplayable will be silently dropped from the return value.
When FALLBACK is 'error, such characters cause an error to be
thrown.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

(fn SEQUENCE &optional FALLBACK TEST)" nil nil) (autoload 'ucs-utils-intact-string "ucs-utils" "Return a string corresponding to SEQUENCE of UCS names or characters.

This function differs from `ucs-utils-string' in that FALLBACK is
a non-optional single string, to be used unless every member of
SEQUENCE exists and passes TEST.  FALLBACK may not be nil, 'error,
or 'drop as in `ucs-utils-string'.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

(fn SEQUENCE FALLBACK &optional TEST)" nil nil) (autoload 'ucs-utils-subst-char-in-region "ucs-utils" "From START to END, replace FROM-CHAR with TO-CHAR each time it occurs.

If optional arg NO-UNDO is non-nil, don't record this change for
undo and don't mark the buffer as really changed.

Characters may be of differing byte-lengths.

The character at the position END is not included, matching the
behavior of `subst-char-in-region'.

This function is slower than `subst-char-in-region'.

(fn START END FROM-CHAR TO-CHAR &optional NO-UNDO)" nil nil) (autoload 'ucs-utils-read-char-by-name "ucs-utils" "Read a character by its Unicode name or hex number string.

A wrapper for `read-char-by-name', with the option to use
`ido-completing-read'.

PROMPT is displayed, and a string that represents a character by
its name is read.

When IDO is set, several seconds are required on the first
run as all completion candidates are pre-generated.

(fn PROMPT &optional IDO)" nil nil) (autoload 'ucs-utils-eval "ucs-utils" "Display a string UCS name for the character at POS.

POS defaults to the current point.

If `transient-mark-mode' is enabled and there is an active
region, return a list of strings UCS names, one for each
character in the region.  If called from Lisp with an
explicit POS, ignores the region.

If called with universal prefix ARG, display the result
in a separate buffer.  If called with two universal
prefix ARGs, replace the current character or region with
its UCS name translation.

(fn &optional POS ARG)" t nil) (autoload 'ucs-utils-ucs-insert "ucs-utils" "Insert CHARACTER in COUNT copies, where CHARACTER is a Unicode code point.

Works like `ucs-insert', with the following differences

    * Uses `ido-completing-read' at the interactive prompt

    * If `transient-mark-mode' is enabled, and the region contains
      a valid UCS character name, that value is used as the
      character name and the region is replaced.

    * A UCS character name string may be passed for CHARACTER.

INHERIT is as documented at `ucs-insert'.

(fn CHARACTER &optional COUNT INHERIT)" t nil) (autoload 'ucs-utils-install-aliases "ucs-utils" "Install aliases outside the \"ucs-utils-\" namespace.

The following aliases will be installed:

    `ucs-char'                  for   `ucs-utils-char'
    `ucs-first-existing-char'   for   `ucs-utils-first-existing-char'
    `ucs-string'                for   `ucs-utils-string'
    `ucs-intact-string'         for   `ucs-utils-intact-string'
    `ucs-vector'                for   `ucs-utils-vector'
    `ucs-pretty-name'           for   `ucs-utils-pretty-name'
    `ucs-eval'                  for   `ucs-utils-eval'" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ucs-utils" '("character-name-history" "persistent-soft" "ucs-utils-"))) (provide 'ucs-utils-autoloads)) "unicode-fonts" ((unicode-fonts unicode-fonts-autoloads) (let ((loads (get 'unicode-fonts 'custom-loads))) (if (member '"unicode-fonts" loads) nil (put 'unicode-fonts 'custom-loads (cons '"unicode-fonts" loads)))) (let ((loads (get 'unicode-fonts-tweaks 'custom-loads))) (if (member '"unicode-fonts" loads) nil (put 'unicode-fonts-tweaks 'custom-loads (cons '"unicode-fonts" loads)))) (let ((loads (get 'unicode-fonts-debug 'custom-loads))) (if (member '"unicode-fonts" loads) nil (put 'unicode-fonts-debug 'custom-loads (cons '"unicode-fonts" loads)))) (autoload 'unicode-fonts-first-existing-font "unicode-fonts" "Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked.

(fn FONT-NAMES)" nil nil) (autoload 'unicode-fonts-font-exists-p "unicode-fonts" "Run `font-utils-exists-p' with a limited scope.

The scope is defined by `unicode-fonts-restrict-to-fonts'.

FONT-NAME, POINT-SIZE, and STRICT are as documented at
`font-utils-exists-p'.

(fn FONT-NAME &optional POINT-SIZE STRICT)" nil nil) (autoload 'unicode-fonts-read-block-name "unicode-fonts" "Read a Unicode block name using `completing-read'.

Spaces are replaced with underscores in completion values, but
are removed from the return value.

Use `ido-completing-read' if IDO is set.

(fn &optional IDO)" nil nil) (autoload 'unicode-fonts-setup "unicode-fonts" "Set up Unicode fonts for FONTSET-NAMES.

Optional FONTSET-NAMES must be a list of strings.  Fontset names
which do not currently exist will be ignored.  The default value
is `unicode-fonts-fontset-names'.

Optional REGENERATE requests that the disk cache be invalidated
and regenerated.

(fn &optional FONTSET-NAMES REGENERATE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "unicode-fonts" '("persistent-softest-" "unicode-"))) (provide 'unicode-fonts-autoloads)) "almost-mono-themes" ((almost-mono-blue-theme almost-mono-gray-theme almost-mono-cream-theme almost-mono-white-theme almost-mono-themes almost-mono-black-theme almost-mono-themes-autoloads) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "almost-mono-themes" '("almost-mono-themes-"))) (provide 'almost-mono-themes-autoloads)) "hide-mode-line" ((hide-mode-line hide-mode-line-autoloads) (autoload 'hide-mode-line-mode "hide-mode-line" "Minor mode to hide the mode-line in the current buffer.

If called interactively, enable Hide-Mode-Line mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-hide-mode-line-mode 'globalized-minor-mode t) (defvar global-hide-mode-line-mode nil "Non-nil if Global Hide-Mode-Line mode is enabled.
See the `global-hide-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hide-mode-line-mode'.") (custom-autoload 'global-hide-mode-line-mode "hide-mode-line" nil) (autoload 'global-hide-mode-line-mode "hide-mode-line" "Toggle Hide-Mode-Line mode in all buffers.
With prefix ARG, enable Global Hide-Mode-Line mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Hide-Mode-Line mode is enabled in all buffers where
`turn-on-hide-mode-line-mode' would do it.
See `hide-mode-line-mode' for more information on Hide-Mode-Line mode.

(fn &optional ARG)" t nil) (autoload 'turn-on-hide-mode-line-mode "hide-mode-line" "Turn on `hide-mode-line-mode'.
Unless in `fundamental-mode' or `hide-mode-line-excluded-modes'." nil nil) (autoload 'turn-off-hide-mode-line-mode "hide-mode-line" "Turn off `hide-mode-line-mode'." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "hide-mode-line" '("hide-mode-line-"))) (provide 'hide-mode-line-autoloads)) "dashboard" ((dashboard-widgets dashboard dashboard-autoloads) (autoload 'dashboard-setup-startup-hook "dashboard" "Setup post initialization hooks.
If a command line argument is provided,
assume a filename and skip displaying Dashboard." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dashboard" '("dashboard-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dashboard-widgets" '("dashboard-" "org-time-less-p" "recentf-list"))) (provide 'dashboard-autoloads)) "org-bullets" ((org-bullets org-bullets-autoloads) (autoload 'org-bullets-mode "org-bullets" "Use UTF8 bullets in Org mode headings.

If called interactively, enable Org-Bullets mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bullets" '("org-bullets-"))) (provide 'org-bullets-autoloads)) "olivetti" ((olivetti-autoloads olivetti) (autoload 'olivetti-mode "olivetti" "Olivetti provides a nice writing environment.
Window margins are set to relative widths to accomodate a text
body width set with `olivetti-body-width'.

If called interactively, enable Olivetti mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "olivetti" '("olivetti-"))) (provide 'olivetti-autoloads)) "emacsql" ((emacsql emacsql-autoloads emacsql-compiler) (autoload 'emacsql-show-last-sql "emacsql" "Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

(fn &optional PREFIX)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql" '("emacsql-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-compiler" '("emacsql-"))) (provide 'emacsql-autoloads)) "emacsql-sqlite" ((emacsql-sqlite emacsql-sqlite-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-sqlite" '("emacsql-sqlite-"))) (provide 'emacsql-sqlite-autoloads)) "org-roam" ((org-roam-autoloads org-roam-graph org-roam org-roam-compat org-roam-capture org-roam-migrate org-roam-node org-roam-dailies org-roam-db org-roam-utils org-roam-protocol org-roam-mode org-roam-overlay) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-"))) (autoload 'org-roam-capture- "org-roam-capture" "Main entry point of `org-roam-capture' module.
GOTO and KEYS correspond to `org-capture' arguments.
INFO is a plist for filling up Org-roam's capture templates.
NODE is an `org-roam-node' construct containing information about the node.
PROPS is a plist containing additional Org-roam properties for each template.
TEMPLATES is a list of org-roam templates.

(fn &key GOTO KEYS NODE INFO PROPS TEMPLATES)" nil nil) (autoload 'org-roam-capture "org-roam-capture" "Launches an `org-capture' process for a new or existing node.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed along to the underlying `org-roam-capture-'.

(fn &optional GOTO KEYS &key FILTER-FN TEMPLATES INFO)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-capture-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-compat" '("org-roam--"))) (autoload 'org-roam-dailies-capture-today "org-roam-dailies" "Create an entry in the daily-note for today.
When GOTO is non-nil, go the note without creating an entry.

(fn &optional GOTO)" t nil) (autoload 'org-roam-dailies-goto-today "org-roam-dailies" "Find the daily-note for today, creating it if necessary." t nil) (autoload 'org-roam-dailies-capture-tomorrow "org-roam-dailies" "Create an entry in the daily-note for tomorrow.

With numeric argument N, use the daily-note N days in the future.

With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

(fn N &optional GOTO)" t nil) (autoload 'org-roam-dailies-goto-tomorrow "org-roam-dailies" "Find the daily-note for tomorrow, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

(fn N)" t nil) (autoload 'org-roam-dailies-capture-yesterday "org-roam-dailies" "Create an entry in the daily-note for yesteday.

With numeric argument N, use the daily-note N days in the past.

When GOTO is non-nil, go the note without creating an entry.

(fn N &optional GOTO)" t nil) (autoload 'org-roam-dailies-goto-yesterday "org-roam-dailies" "Find the daily-note for yesterday, creating it if necessary.

With numeric argument N, use the daily-note N days in the
future.

(fn N)" t nil) (autoload 'org-roam-dailies-capture-date "org-roam-dailies" "Create an entry in the daily-note for a date using the calendar.
Prefer past dates, unless PREFER-FUTURE is non-nil.
With a `C-u' prefix or when GOTO is non-nil, go the note without
creating an entry.

(fn &optional GOTO PREFER-FUTURE)" t nil) (autoload 'org-roam-dailies-goto-date "org-roam-dailies" "Find the daily-note for a date using the calendar, creating it if necessary.
Prefer past dates, unless PREFER-FUTURE is non-nil.

(fn &optional PREFER-FUTURE)" t nil) (autoload 'org-roam-dailies-find-directory "org-roam-dailies" "Find and open `org-roam-dailies-directory'." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-"))) (autoload 'org-roam-db-sync "org-roam-db" "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch.

(fn &optional FORCE)" t nil) (defvar org-roam-db-autosync-mode nil "Non-nil if Org-Roam-Db-Autosync mode is enabled.
See the `org-roam-db-autosync-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-db-autosync-mode'.") (custom-autoload 'org-roam-db-autosync-mode "org-roam-db" nil) (autoload 'org-roam-db-autosync-mode "org-roam-db" "Global minor mode to keep your Org-roam session automatically synchronized.
Through the session this will continue to setup your
buffers (that are Org-roam file visiting), keep track of the
related changes, maintain cache consistency and incrementally
update the currently active database.

If called interactively, enable Org-Roam-Db-Autosync mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

If you need to manually trigger resync of the currently active
database, see `org-roam-db-sync' command.

(fn &optional ARG)" t nil) (autoload 'org-roam-db-autosync-enable "org-roam-db" "Activate `org-roam-db-autosync-mode'." nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("emacsql-constraint" "org-roam-"))) (autoload 'org-roam-graph "org-roam-graph" "Build and possibly display a graph for NODE.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for NODE.
  - `\\[universal-argument]' N   show the graph for NODE limiting nodes to N steps.

(fn &optional ARG NODE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-"))) (autoload 'org-roam-migrate-wizard "org-roam-migrate" "Migrate all notes from to be compatible with Org-roam v2.
1. Convert all notes from v1 format to v2.
2. Rebuild the cache.
3. Replace all file links with ID links." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-migrate" '("org-roam-migrate-"))) (autoload 'org-roam-buffer-display-dedicated "org-roam-mode" "Launch NODE dedicated Org-roam buffer.
Unlike the persistent `org-roam-buffer', the contents of this
buffer won't be automatically changed and will be held in place.

In interactive calls prompt to select NODE, unless called with
`universal-argument', in which case NODE will be set to
`org-roam-node-at-point'.

(fn NODE)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-mode" '("org-roam-"))) (autoload 'org-roam-node-find "org-roam-node" "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
If OTHER-WINDOW, visit the NODE in another window.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)

(fn &optional OTHER-WINDOW INITIAL-INPUT FILTER-FN &key TEMPLATES)" t nil) (autoload 'org-roam-node-random "org-roam-node" "Find and open a random Org-roam node.
With prefix argument OTHER-WINDOW, visit the node in another
window instead.

(fn &optional OTHER-WINDOW)" t nil) (autoload 'org-roam-node-insert "org-roam-node" "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'.

(fn &optional FILTER-FN &key TEMPLATES INFO)" t nil) (autoload 'org-roam-refile "org-roam-node" "Refile node at point to an Org-roam node.
If region is active, then use it instead of the node at point." t nil) (autoload 'org-roam-extract-subtree "org-roam-node" "Convert current subtree at point to a node, and extract it into a new file." t nil) (autoload 'org-roam-update-org-id-locations "org-roam-node" "Scan Org-roam files to update `org-id' related state.
This is like `org-id-update-id-locations', but will automatically
use the currently bound `org-directory' and `org-roam-directory'
along with DIRECTORIES (if any), where the lookup for files in
these directories will be always recursive.

Note: Org-roam doesn't have hard dependency on
`org-id-locations-file' to lookup IDs for nodes that are stored
in the database, but it still tries to properly integrates with
`org-id'. This allows the user to cross-reference IDs outside of
the current `org-roam-directory', and also link with \"id:\"
links to headings/files within the current `org-roam-directory'
that are excluded from identification in Org-roam as
`org-roam-node's, e.g. with \"ROAM_EXCLUDE\" property.

(fn &rest DIRECTORIES)" t nil) (autoload 'org-roam-ref-find "org-roam-node" "Find and open an Org-roam node that's dedicated to a specific ref.
INITIAL-INPUT is the initial input to the prompt.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.

(fn &optional INITIAL-INPUT FILTER-FN)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-node" '("org-roam-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-overlay" '("org-roam-overlay-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-"))) (autoload 'org-roam-version "org-roam-utils" "Return `org-roam' version.
Interactively, or when MESSAGE is non-nil, show in the echo area.

(fn &optional MESSAGE)" t nil) (autoload 'org-roam-diagnostics "org-roam-utils" "Collect and print info for `org-roam' issues." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-utils" '("org-roam-"))) (provide 'org-roam-autoloads)) "deft" ((deft deft-autoloads) (autoload 'deft-find-file "deft" "Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

(fn FILE)" t nil) (autoload 'deft-new-file "deft" "Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title." t nil) (autoload 'deft "deft" "Switch to *Deft* buffer and load files." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deft" '("deft-" "org-deft-store-link"))) (provide 'deft-autoloads)) "mixed-pitch" ((mixed-pitch-autoloads mixed-pitch) (autoload 'mixed-pitch-mode "mixed-pitch" "Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.

If called interactively, enable Mixed-Pitch mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mixed-pitch" '("mixed-pitch-"))) (provide 'mixed-pitch-autoloads)) "emojify" ((emojify-autoloads emojify) (autoload 'emojify-set-emoji-styles "emojify" "Set the type of emojis that should be displayed.

STYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'

(fn STYLES)" nil nil) (autoload 'emojify-mode "emojify" "Emojify mode

If called interactively, enable Emojify mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-emojify-mode 'globalized-minor-mode t) (defvar global-emojify-mode nil "Non-nil if Global Emojify mode is enabled.
See the `global-emojify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode'.") (custom-autoload 'global-emojify-mode "emojify" nil) (autoload 'global-emojify-mode "emojify" "Toggle Emojify mode in all buffers.
With prefix ARG, enable Global Emojify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify mode is enabled in all buffers where
`emojify-mode' would do it.
See `emojify-mode' for more information on Emojify mode.

(fn &optional ARG)" t nil) (autoload 'emojify-mode-line-mode "emojify" "Emojify mode line

If called interactively, enable Emojify-Mode-Line mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (put 'global-emojify-mode-line-mode 'globalized-minor-mode t) (defvar global-emojify-mode-line-mode nil "Non-nil if Global Emojify-Mode-Line mode is enabled.
See the `global-emojify-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode-line-mode'.") (custom-autoload 'global-emojify-mode-line-mode "emojify" nil) (autoload 'global-emojify-mode-line-mode "emojify" "Toggle Emojify-Mode-Line mode in all buffers.
With prefix ARG, enable Global Emojify-Mode-Line mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify-Mode-Line mode is enabled in all buffers where
`emojify-mode-line-mode' would do it.
See `emojify-mode-line-mode' for more information on Emojify-Mode-Line mode.

(fn &optional ARG)" t nil) (autoload 'emojify-apropos-emoji "emojify" "Show Emojis that match PATTERN.

(fn PATTERN)" t nil) (autoload 'emojify-insert-emoji "emojify" "Interactively prompt for Emojis and insert them in the current buffer.

This respects the `emojify-emoji-styles' variable." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emojify" '("emojify-"))) (provide 'emojify-autoloads)) "org-superstar" ((org-superstar-autoloads org-superstar) (put 'org-superstar-leading-bullet 'safe-local-variable #'char-or-string-p) (autoload 'org-superstar-toggle-lightweight-lists "org-superstar" "Toggle syntax checking for plain list items.

Disabling syntax checking will cause Org Superstar to display
lines looking like plain lists (for example in code) like plain
lists.  However, this may cause significant speedup for org files
containing several hundred list items." t nil) (autoload 'org-superstar-mode "org-superstar" "Use UTF8 bullets for headlines and plain lists.

If called interactively, enable Org-Superstar mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

(fn &optional ARG)" t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-superstar" '("org-superstar-"))) (provide 'org-superstar-autoloads)) "org-habit-plus" ((org-habit-plus org-habit-plus-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-habit-plus" '("org-"))) (provide 'org-habit-plus-autoloads)) "org-dashboard" ((org-dashboard-autoloads org-dashboard) (autoload 'org-dashboard-display "org-dashboard" nil t nil) (autoload 'org-dblock-write:block-dashboard "org-dashboard" "Generate a progress report inside an org dynamic block.

Progress information is retrieved by searching files in
`org-dashboard-files' for headings containing a \"progress cookie\",
e.g.:

  ** [50%] release v0.1
  *** TODO publish on github
  *** DONE import in git

See Info node `(org) Breaking down tasks'.

(fn PARAMS)" nil nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-dashboard" '("org-dashboard-"))) (provide 'org-dashboard-autoloads)) "borland-blue-theme" ((borland-theme borland-blue-theme-autoloads borland-blue-theme) (when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "borland-theme" '("borland-theme-"))) (provide 'borland-blue-theme-autoloads)) "base16-theme" ((base16-porple-theme base16-atelier-forest-theme base16-eighties-theme base16-black-metal-marduk-theme base16-3024-theme base16-tango-theme base16-black-metal-dark-funeral-theme base16-horizon-terminal-light-theme base16-tube-theme base16-tomorrow-night-eighties-theme base16-isotope-theme base16-default-dark-theme base16-atelier-heath-light-theme base16-summerfruit-light-theme base16-theme-pkg base16-synth-midnight-light-theme base16-material-palenight-theme base16-embers-theme base16-atelier-lakeside-theme base16-heetch-light-theme base16-bright-theme base16-cupcake-theme base16-atelier-dune-light-theme base16-pop-theme base16-black-metal-mayhem-theme base16-tomorrow-night-theme base16-theme-autoloads base16-silk-light-theme base16-shapeshifter-theme base16-black-metal-burzum-theme base16-ashes-theme base16-black-metal-bathory-theme base16-railscasts-theme base16-framer-theme base16-atelier-savanna-theme base16-helios-theme base16-phd-theme base16-brushtrees-theme base16-apathy-theme base16-atelier-seaside-light-theme base16-tender-theme base16-atelier-seaside-theme base16-twilight-theme base16-chalk-theme base16-xcode-dusk-theme base16-windows-highcontrast-theme base16-oceanicnext-theme base16-tomorrow-theme base16-one-light-theme base16-theme base16-gruvbox-dark-hard-theme base16-synth-midnight-dark-theme base16-nova-theme base16-rose-pine-theme base16-harmonic-light-theme base16-cupertino-theme base16-greenscreen-theme base16-gruvbox-light-soft-theme base16-woodland-theme base16-atelier-estuary-light-theme base16-harmonic-dark-theme base16-solarflare-light-theme base16-gruvbox-dark-pale-theme base16-gruvbox-light-hard-theme base16-darkviolet-theme base16-monokai-theme base16-nebula-theme base16-brushtrees-dark-theme base16-atelier-plateau-light-theme base16-material-darker-theme base16-gruvbox-light-medium-theme base16-equilibrium-light-theme base16-summercamp-theme base16-solarized-dark-theme base16-dirtysea-theme base16-grayscale-dark-theme base16-darcula-theme base16-hardcore-theme base16-gruvbox-dark-medium-theme base16-kimber-theme base16-codeschool-theme base16-vulcan-theme base16-humanoid-dark-theme base16-zenburn-theme base16-material-theme base16-flat-theme base16-atelier-lakeside-light-theme base16-materia-theme base16-seti-theme base16-atelier-sulphurpool-light-theme base16-onedark-theme base16-material-lighter-theme base16-atelier-cave-light-theme base16-hopscotch-theme base16-black-metal-gorgoroth-theme base16-macintosh-theme base16-humanoid-light-theme base16-atelier-savanna-light-theme base16-eva-theme base16-atelier-cave-theme base16-brewer-theme base16-horizon-terminal-dark-theme base16-gruvbox-dark-soft-theme base16-black-metal-khold-theme base16-summerfruit-dark-theme base16-atelier-heath-theme base16-pasque-theme base16-classic-dark-theme base16-equilibrium-gray-light-theme base16-atelier-plateau-theme base16-atelier-sulphurpool-theme base16-google-dark-theme base16-equilibrium-dark-theme base16-ia-light-theme base16-darkmoss-theme base16-grayscale-light-theme base16-espresso-theme base16-google-light-theme base16-papercolor-light-theme base16-github-theme base16-mocha-theme base16-fruit-soda-theme base16-atlas-theme base16-decaf-theme base16-marrakesh-theme base16-circus-theme base16-black-metal-nile-theme base16-ia-dark-theme base16-windows-95-light-theme base16-spacemacs-theme base16-icy-theme base16-equilibrium-gray-dark-theme base16-darktooth-theme base16-windows-nt-theme base16-apprentice-theme base16-horizon-light-theme base16-black-metal-theme base16-sagelight-theme base16-atelier-estuary-theme base16-silk-dark-theme base16-irblack-theme base16-nord-theme base16-windows-95-theme base16-ocean-theme base16-edge-dark-theme base16-black-metal-venom-theme base16-solarized-light-theme base16-horizon-dark-theme base16-danqing-theme base16-edge-light-theme base16-paraiso-theme base16-rebecca-theme base16-unikitty-light-theme base16-windows-highcontrast-light-theme base16-dracula-theme base16-pico-theme base16-bespin-theme base16-windows-10-light-theme base16-rose-pine-moon-theme base16-sandcastle-theme base16-papercolor-dark-theme base16-atelier-dune-theme base16-black-metal-immortal-theme base16-mexico-light-theme base16-snazzy-theme base16-outrun-dark-theme base16-mellow-purple-theme base16-windows-nt-light-theme base16-eva-dim-theme base16-classic-light-theme base16-windows-10-theme base16-rose-pine-dawn-theme base16-unikitty-dark-theme base16-gigavolt-theme base16-material-vivid-theme base16-brogrammer-theme base16-default-light-theme base16-atelier-forest-light-theme base16-solarflare-theme base16-heetch-theme) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-3024-theme" '("base16-3024-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-apathy-theme" '("base16-apathy-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-apprentice-theme" '("base16-apprentice-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-ashes-theme" '("base16-ashes-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-cave-light-theme" '("base16-atelier-cave-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-cave-theme" '("base16-atelier-cave-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-dune-light-theme" '("base16-atelier-dune-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-dune-theme" '("base16-atelier-dune-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-estuary-light-theme" '("base16-atelier-estuary-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-estuary-theme" '("base16-atelier-estuary-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-forest-light-theme" '("base16-atelier-forest-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-forest-theme" '("base16-atelier-forest-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-heath-light-theme" '("base16-atelier-heath-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-heath-theme" '("base16-atelier-heath-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-lakeside-light-theme" '("base16-atelier-lakeside-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-lakeside-theme" '("base16-atelier-lakeside-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-plateau-light-theme" '("base16-atelier-plateau-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-plateau-theme" '("base16-atelier-plateau-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-savanna-light-theme" '("base16-atelier-savanna-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-savanna-theme" '("base16-atelier-savanna-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-seaside-light-theme" '("base16-atelier-seaside-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-seaside-theme" '("base16-atelier-seaside-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-sulphurpool-light-theme" '("base16-atelier-sulphurpool-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atelier-sulphurpool-theme" '("base16-atelier-sulphurpool-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-atlas-theme" '("base16-atlas-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-bespin-theme" '("base16-bespin-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-bathory-theme" '("base16-black-metal-bathory-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-burzum-theme" '("base16-black-metal-burzum-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-dark-funeral-theme" '("base16-black-metal-dark-funeral-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-gorgoroth-theme" '("base16-black-metal-gorgoroth-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-immortal-theme" '("base16-black-metal-immortal-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-khold-theme" '("base16-black-metal-khold-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-marduk-theme" '("base16-black-metal-marduk-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-mayhem-theme" '("base16-black-metal-mayhem-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-nile-theme" '("base16-black-metal-nile-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-theme" '("base16-black-metal-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-black-metal-venom-theme" '("base16-black-metal-venom-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-brewer-theme" '("base16-brewer-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-bright-theme" '("base16-bright-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-brogrammer-theme" '("base16-brogrammer-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-brushtrees-dark-theme" '("base16-brushtrees-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-brushtrees-theme" '("base16-brushtrees-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-chalk-theme" '("base16-chalk-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-circus-theme" '("base16-circus-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-classic-dark-theme" '("base16-classic-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-classic-light-theme" '("base16-classic-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-codeschool-theme" '("base16-codeschool-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-cupcake-theme" '("base16-cupcake-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-cupertino-theme" '("base16-cupertino-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-danqing-theme" '("base16-danqing-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-darcula-theme" '("base16-darcula-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-darkmoss-theme" '("base16-darkmoss-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-darktooth-theme" '("base16-darktooth-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-darkviolet-theme" '("base16-darkviolet-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-decaf-theme" '("base16-decaf-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-default-dark-theme" '("base16-default-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-default-light-theme" '("base16-default-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-dirtysea-theme" '("base16-dirtysea-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-dracula-theme" '("base16-dracula-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-edge-dark-theme" '("base16-edge-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-edge-light-theme" '("base16-edge-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-eighties-theme" '("base16-eighties-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-embers-theme" '("base16-embers-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-equilibrium-dark-theme" '("base16-equilibrium-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-equilibrium-gray-dark-theme" '("base16-equilibrium-gray-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-equilibrium-gray-light-theme" '("base16-equilibrium-gray-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-equilibrium-light-theme" '("base16-equilibrium-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-espresso-theme" '("base16-espresso-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-eva-dim-theme" '("base16-eva-dim-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-eva-theme" '("base16-eva-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-flat-theme" '("base16-flat-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-framer-theme" '("base16-framer-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-fruit-soda-theme" '("base16-fruit-soda-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gigavolt-theme" '("base16-gigavolt-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-github-theme" '("base16-github-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-google-dark-theme" '("base16-google-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-google-light-theme" '("base16-google-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-grayscale-dark-theme" '("base16-grayscale-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-grayscale-light-theme" '("base16-grayscale-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-greenscreen-theme" '("base16-greenscreen-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-dark-hard-theme" '("base16-gruvbox-dark-hard-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-dark-medium-theme" '("base16-gruvbox-dark-medium-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-dark-pale-theme" '("base16-gruvbox-dark-pale-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-dark-soft-theme" '("base16-gruvbox-dark-soft-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-light-hard-theme" '("base16-gruvbox-light-hard-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-light-medium-theme" '("base16-gruvbox-light-medium-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-gruvbox-light-soft-theme" '("base16-gruvbox-light-soft-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-hardcore-theme" '("base16-hardcore-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-harmonic-dark-theme" '("base16-harmonic-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-harmonic-light-theme" '("base16-harmonic-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-heetch-light-theme" '("base16-heetch-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-heetch-theme" '("base16-heetch-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-helios-theme" '("base16-helios-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-hopscotch-theme" '("base16-hopscotch-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-horizon-dark-theme" '("base16-horizon-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-horizon-light-theme" '("base16-horizon-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-horizon-terminal-dark-theme" '("base16-horizon-terminal-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-horizon-terminal-light-theme" '("base16-horizon-terminal-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-humanoid-dark-theme" '("base16-humanoid-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-humanoid-light-theme" '("base16-humanoid-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-ia-dark-theme" '("base16-ia-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-ia-light-theme" '("base16-ia-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-icy-theme" '("base16-icy-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-irblack-theme" '("base16-irblack-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-isotope-theme" '("base16-isotope-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-kimber-theme" '("base16-kimber-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-macintosh-theme" '("base16-macintosh-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-marrakesh-theme" '("base16-marrakesh-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-materia-theme" '("base16-materia-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-material-darker-theme" '("base16-material-darker-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-material-lighter-theme" '("base16-material-lighter-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-material-palenight-theme" '("base16-material-palenight-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-material-theme" '("base16-material-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-material-vivid-theme" '("base16-material-vivid-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-mellow-purple-theme" '("base16-mellow-purple-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-mexico-light-theme" '("base16-mexico-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-mocha-theme" '("base16-mocha-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-monokai-theme" '("base16-monokai-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-nebula-theme" '("base16-nebula-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-nord-theme" '("base16-nord-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-nova-theme" '("base16-nova-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-ocean-theme" '("base16-ocean-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-oceanicnext-theme" '("base16-oceanicnext-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-one-light-theme" '("base16-one-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-onedark-theme" '("base16-onedark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-outrun-dark-theme" '("base16-outrun-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-papercolor-dark-theme" '("base16-papercolor-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-papercolor-light-theme" '("base16-papercolor-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-paraiso-theme" '("base16-paraiso-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-pasque-theme" '("base16-pasque-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-phd-theme" '("base16-phd-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-pico-theme" '("base16-pico-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-pop-theme" '("base16-pop-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-porple-theme" '("base16-porple-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-railscasts-theme" '("base16-railscasts-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-rebecca-theme" '("base16-rebecca-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-rose-pine-dawn-theme" '("base16-rose-pine-dawn-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-rose-pine-moon-theme" '("base16-rose-pine-moon-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-rose-pine-theme" '("base16-rose-pine-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-sagelight-theme" '("base16-sagelight-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-sandcastle-theme" '("base16-sandcastle-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-seti-theme" '("base16-seti-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-shapeshifter-theme" '("base16-shapeshifter-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-silk-dark-theme" '("base16-silk-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-silk-light-theme" '("base16-silk-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-snazzy-theme" '("base16-snazzy-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-solarflare-light-theme" '("base16-solarflare-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-solarflare-theme" '("base16-solarflare-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-solarized-dark-theme" '("base16-solarized-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-solarized-light-theme" '("base16-solarized-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-spacemacs-theme" '("base16-spacemacs-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-summercamp-theme" '("base16-summercamp-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-summerfruit-dark-theme" '("base16-summerfruit-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-summerfruit-light-theme" '("base16-summerfruit-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-synth-midnight-dark-theme" '("base16-synth-midnight-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-synth-midnight-light-theme" '("base16-synth-midnight-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-tango-theme" '("base16-tango-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-tender-theme" '("base16-tender-colors"))) (and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-theme" '("base16-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-tomorrow-night-eighties-theme" '("base16-tomorrow-night-eighties-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-tomorrow-night-theme" '("base16-tomorrow-night-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-tomorrow-theme" '("base16-tomorrow-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-tube-theme" '("base16-tube-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-twilight-theme" '("base16-twilight-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-unikitty-dark-theme" '("base16-unikitty-dark-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-unikitty-light-theme" '("base16-unikitty-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-vulcan-theme" '("base16-vulcan-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-10-light-theme" '("base16-windows-10-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-10-theme" '("base16-windows-10-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-95-light-theme" '("base16-windows-95-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-95-theme" '("base16-windows-95-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-highcontrast-light-theme" '("base16-windows-highcontrast-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-highcontrast-theme" '("base16-windows-highcontrast-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-nt-light-theme" '("base16-windows-nt-light-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-windows-nt-theme" '("base16-windows-nt-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-woodland-theme" '("base16-woodland-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-xcode-dusk-theme" '("base16-xcode-dusk-colors"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "base16-zenburn-theme" '("base16-zenburn-colors"))) (provide 'base16-theme-autoloads)) "basic-theme" ((basic-theme-autoloads basic-theme) (and load-file-name (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "basic-theme" '("basic"))) (provide 'basic-theme-autoloads)) "color-theme-sanityinc-tomorrow" ((sanityinc-tomorrow-bright-theme color-theme-sanityinc-tomorrow-autoloads sanityinc-tomorrow-blue-theme color-theme-sanityinc-tomorrow sanityinc-tomorrow-night-theme sanityinc-tomorrow-day-theme sanityinc-tomorrow-eighties-theme) (when (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name)))) (autoload 'color-theme-sanityinc-tomorrow-night "color-theme-sanityinc-tomorrow" "Apply the tomorrow night theme." t nil) (autoload 'color-theme-sanityinc-tomorrow-day "color-theme-sanityinc-tomorrow" "Apply the tomorrow day theme." t nil) (autoload 'color-theme-sanityinc-tomorrow-bright "color-theme-sanityinc-tomorrow" "Apply the tomorrow bright theme." t nil) (autoload 'color-theme-sanityinc-tomorrow-eighties "color-theme-sanityinc-tomorrow" "Apply the tomorrow eighties theme." t nil) (autoload 'color-theme-sanityinc-tomorrow-blue "color-theme-sanityinc-tomorrow" "Apply the tomorrow blue theme." t nil) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "color-theme-sanityinc-tomorrow" '("color-theme-sanityinc-tomorrow" "sanityinc-tomorrow--"))) (provide 'color-theme-sanityinc-tomorrow-autoloads)) "promise" ((promise promise-autoloads promise-done promise-es6-extensions promise-core promise-finally promise-rejection-tracking) (autoload 'promise-chain "promise" "Extract PROMISE, BODY include then, catch, done and finally.

Extract the following code...

    (promise-chain (promise-new ...)
      (then
       (lambda (value)
         ...))

      (catch
       (lambda (reason)
         ...))

      (done
       (lambda (value)
         ...))

      (finally
       (lambda () ...))

      ;; Anaphoric versions of `then' and `catch'.

      (thena (message \"result -> %s\" result)
             ...)

      (catcha (message \"error: reason -> %s\" reason)
              ...))

as below.

    (let ((promise (promise-new ...)))
      (setf promise (promise-then promise
                                  (lambda (value)
                                    ...)))

      (setf promise (promise-catch promise
                                   (lambda (value)
                                     ...)))

      (setf promise (promise-done promise
                                  (lambda (reason)
                                    ...)))

      (setf promise (promise-finally promise
                                     (lambda ()
                                       ...)))

      (setf promise (promise-then promise
                                  (lambda (result)
                                    (message \"result -> %s\" result)
                                    ...)))

      (setf promise (promise-catch promise
                                   (lambda (reason)
                                     (message \"error: reason -> %s\" reason)
                                     ...)))
      promise)

(fn PROMISE &rest BODY)" nil t) (function-put 'promise-chain 'lisp-indent-function '1) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise" '("promise"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-core" '("promise-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-es6-extensions" '("promise-"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-rejection-tracking" '("promise-"))) (provide 'promise-autoloads)) "iter2" ((iter2 iter2-autoloads) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "iter2" '("iter2-"))) (provide 'iter2-autoloads)) "async-await" ((async-await-autoloads async-await) (autoload 'async-defun "async-await" "Define NAME as a Async Function which return Promise.
ARGLIST should take the same form as an argument list for a `defun'.
BODY should be a list of Lisp expressions.

 (defun wait-async (n)
   (promise-new (lambda (resolve _reject)
                  (run-at-time n
                               nil
                               (lambda ()
                                 (funcall resolve n))))))

 (async-defun foo-async ()
   (print (await (wait-async 0.5)))
   (message \"---\")

   (print (await (wait-async 1.0)))
   (message \"---\")

   (print (await (wait-async 1.5)))
   (message \"---\")

   (message \"await done\"))

 (foo-async)

(fn NAME ARGLIST &rest BODY)" nil t) (function-put 'async-defun 'doc-string-elt '3) (function-put 'async-defun 'lisp-indent-function '2) (autoload 'async-lambda "async-await" "Return a lambda Async Function which return Promise.
ARGLIST should take the same form as an argument list for a `defun'.
BODY should be a list of Lisp expressions.

 (defun wait-async (n)
   (promise-new (lambda (resolve _reject)
                  (run-at-time n
                               nil
                               (lambda ()
                                 (funcall resolve n))))))

 (setq foo-async (async-lambda ()
                   (print (await (wait-async 0.5)))
                   (message \"---\")

                   (print (await (wait-async 1.0)))
                   (message \"---\")

                   (print (await (wait-async 1.5)))
                   (message \"---\")

                   (message \"await done\")))

 (funcall foo-async)

(fn ARGLIST &rest BODY)" nil t) (function-put 'async-lambda 'doc-string-elt '2) (function-put 'async-lambda 'lisp-indent-function 'defun) (autoload 'async-await-advice-make-autoload "async-await" "Advice function for `make-autoload'.
FN is original function and ARGS is list of arguments.
See \"For complex cases\" section in `make-autoload'.

(fn FN &rest ARGS)" nil nil) (advice-add 'make-autoload :around #'async-await-advice-make-autoload) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "async-await" '("async-await-"))) (provide 'async-await-autoloads)) "rocket-chat" ((rocket-chat rocket-chat-autoloads rocket-chat-api) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rocket-chat" '("async-rc-update-channel" "rc-" "rocket-chat"))) (if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rocket-chat-api" '("auth-" "bool-to-str" "cha" "exec-form" "get-json" "groups-" "im-" "integrations-" "json-" "livechat-" "log" "post-json" "reg-info" "setting" "statistics" "url-concat" "users-"))) (provide 'rocket-chat-autoloads))))

#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (org-elpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 11 "melpa" nil "gnu-elpa-mirror" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "nano-emacs" nil "nano-theme" nil "use-package" nil "bind-key" nil "diminish" nil "counsel" nil "ivy" nil "swiper" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "org-gcal" nil "request" nil "request-deferred" nil "deferred" nil "alert" nil "gntp" nil "log4e" nil "cl-lib" nil "persist" nil "ibuffer" nil "dired" nil "undo-tree" nil "guide-key" nil "dash" nil "popwin" nil "s" nil "projectile" nil "key-chord" nil "company" nil "company-c-headers" nil "semantic" nil "irony" nil "json" nil "cc-mode" nil "magit" nil "git-commit" nil "transient" nil "with-editor" nil "magit-section" nil "perl" nil "beacon" nil "seq" nil "smartparens" nil "align" nil "auctex" nil "ace-window" nil "avy" nil "multiple-cursors" nil "flycheck" nil "pkg-info" nil "epl" nil "let-alist" nil "flycheck-irony" nil "flyspell" nil "tide" nil "typescript-mode" nil "rustic" nil "rust-mode" nil "f" nil "markdown-mode" nil "project" nil "xref" nil "spinner" nil "xterm-color" nil "lsp-mode" nil "ht" nil "lv" nil "lsp-ui" nil "all-the-icons" nil "exwm" nil "xelb" nil "cl-generic" nil "selectrum" nil "selectrum-prescient" nil "prescient" nil "consult" nil "marginalia" nil "vertico" nil "orderless" nil "savehist" nil "corfu" nil "ligature" nil "unicode-fonts" nil "font-utils" nil "persistent-soft" nil "pcache" nil "list-utils" nil "ucs-utils" nil "almost-mono-themes" nil "hide-mode-line" nil "dashboard" nil "org-bullets" nil "ido" nil "eshell" nil "olivetti" nil "org-roam" nil "emacsql" nil "emacsql-sqlite" nil "deft" nil "mixed-pitch" nil "emojify" nil "org-superstar" nil "whitespace" nil "mu4e" nil "gnus" nil "org-habit-plus" nil "org-dashboard" nil "borland-blue-theme" nil "borland-theme" nil "base16-theme" nil "simple-theme" nil "basic-theme" nil "color-themes-sanityinc-tomorrow" nil "color-theme-sanityinc-tomorrow" nil "rocket-chat" nil "promise" nil "async-await" nil "iter2" nil "ts-org-interaction" nil "shr" nil)) melpa #s(hash-table size 145 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "gnu-elpa-mirror" nil "el-get" (el-get :type git :flavor melpa :files ("*.el" ("recipes" "recipes/el-get.rcp") "methods" "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "nano-emacs" nil "nano-theme" nil "use-package" (use-package :type git :flavor melpa :files (:defaults (:exclude "bind-key.el" "bind-chord.el" "use-package-chords.el" "use-package-ensure-system-package.el") "use-package-pkg.el") :host github :repo "jwiegley/use-package") "bind-key" (bind-key :type git :flavor melpa :files ("bind-key.el" "bind-key-pkg.el") :host github :repo "jwiegley/use-package") "diminish" (diminish :type git :flavor melpa :host github :repo "myrjola/diminish.el") "counsel" (counsel :type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper") "ivy" (ivy :type git :flavor melpa :files (:defaults (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el") "doc/ivy-help.org" "ivy-pkg.el") :host github :repo "abo-abo/swiper") "swiper" (swiper :type git :flavor melpa :files ("swiper.el" "swiper-pkg.el") :host github :repo "abo-abo/swiper") "org-gcal" (org-gcal :type git :flavor melpa :host github :repo "kidd/org-gcal.el") "request" (request :type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request") "request-deferred" (request-deferred :type git :flavor melpa :files ("request-deferred.el" "request-deferred-pkg.el") :host github :repo "tkf/emacs-request") "deferred" (deferred :type git :flavor melpa :files ("deferred.el" "deferred-pkg.el") :host github :repo "kiwanami/emacs-deferred") "alert" (alert :type git :flavor melpa :host github :repo "jwiegley/alert") "gntp" (gntp :type git :flavor melpa :host github :repo "tekai/gntp.el") "log4e" (log4e :type git :flavor melpa :host github :repo "aki2o/log4e") "cl-lib" nil "persist" nil "ibuffer" nil "dired" nil "undo-tree" nil "guide-key" (guide-key :type git :flavor melpa :host github :repo "kai2nenobu/guide-key") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "popwin" (popwin :type git :flavor melpa :host github :repo "emacsorphanage/popwin") "s" (s :type git :flavor melpa :files ("s.el" "s-pkg.el") :host github :repo "magnars/s.el") "projectile" (projectile :type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile") "key-chord" (key-chord :type git :flavor melpa :host github :repo "emacsorphanage/key-chord") "company" (company :type git :flavor melpa :files (:defaults "icons" "company-pkg.el") :host github :repo "company-mode/company-mode") "company-c-headers" (company-c-headers :type git :flavor melpa :host github :repo "randomphrase/company-c-headers") "semantic" nil "irony" (irony :type git :flavor melpa :files ("*.el" "server" "irony-pkg.el") :host github :repo "Sarcasm/irony-mode") "json" nil "cc-mode" nil "magit" (magit :type git :flavor melpa :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el" "Documentation/magit.texi" "Documentation/AUTHORS.md" "LICENSE" (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el") "magit-pkg.el") :host github :repo "magit/magit") "git-commit" (git-commit :type git :flavor melpa :files ("lisp/git-commit.el" "lisp/git-commit-pkg.el" "git-commit-pkg.el") :host github :repo "magit/magit") "transient" (transient :type git :flavor melpa :files ("lisp/*.el" "docs/transient.texi" "transient-pkg.el") :host github :repo "magit/transient") "with-editor" (with-editor :type git :flavor melpa :host github :repo "magit/with-editor") "magit-section" (magit-section :type git :flavor melpa :files ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "Documentation/magit-section.texi" "magit-section-pkg.el") :host github :repo "magit/magit") "perl" nil "beacon" (beacon :type git :flavor melpa :host github :repo "Malabarba/beacon") "seq" nil "smartparens" (smartparens :type git :flavor melpa :host github :repo "Fuco1/smartparens") "align" nil "auctex" nil "ace-window" (ace-window :type git :flavor melpa :host github :repo "abo-abo/ace-window") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "multiple-cursors" (multiple-cursors :type git :flavor melpa :host github :repo "magnars/multiple-cursors.el") "flycheck" (flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck") "pkg-info" (pkg-info :type git :flavor melpa :host github :repo "emacsorphanage/pkg-info") "epl" (epl :type git :flavor melpa :host github :repo "cask/epl") "let-alist" nil "flycheck-irony" (flycheck-irony :type git :flavor melpa :host github :repo "Sarcasm/flycheck-irony") "flyspell" nil "tide" (tide :type git :flavor melpa :files (:defaults "tsserver" "tide-pkg.el") :host github :repo "ananthakumaran/tide") "typescript-mode" (typescript-mode :type git :flavor melpa :host github :repo "emacs-typescript/typescript.el") "rustic" (rustic :type git :flavor melpa :host github :repo "brotzeit/rustic") "rust-mode" (rust-mode :type git :flavor melpa :host github :repo "rust-lang/rust-mode") "f" (f :type git :flavor melpa :files ("f.el" "f-pkg.el") :host github :repo "rejeep/f.el") "markdown-mode" (markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode") "project" nil "xref" nil "spinner" nil "xterm-color" (xterm-color :type git :flavor melpa :host github :repo "atomontage/xterm-color") "lsp-mode" (lsp-mode :type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode") "ht" (ht :type git :flavor melpa :files ("ht.el" "ht-pkg.el") :host github :repo "Wilfred/ht.el") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "lsp-ui" (lsp-ui :type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el") :host github :repo "emacs-lsp/lsp-ui") "all-the-icons" (all-the-icons :type git :flavor melpa :files (:defaults "data" "all-the-icons-pkg.el") :host github :repo "domtronn/all-the-icons.el") "exwm" nil "xelb" nil "cl-generic" nil "selectrum" (selectrum :type git :flavor melpa :host github :repo "raxod502/selectrum") "selectrum-prescient" (selectrum-prescient :type git :flavor melpa :files ("selectrum-prescient.el" "selectrum-prescient-pkg.el") :host github :repo "raxod502/prescient.el") "prescient" (prescient :type git :flavor melpa :files ("prescient.el" "prescient-pkg.el") :host github :repo "raxod502/prescient.el") "consult" (consult :type git :flavor melpa :host github :repo "minad/consult") "marginalia" (marginalia :type git :flavor melpa :host github :repo "minad/marginalia") "vertico" nil "orderless" (orderless :type git :flavor melpa :host github :repo "oantolin/orderless") "savehist" nil "corfu" nil "ligature" nil "unicode-fonts" (unicode-fonts :type git :flavor melpa :host github :repo "rolandwalker/unicode-fonts") "font-utils" (font-utils :type git :flavor melpa :host github :repo "rolandwalker/font-utils") "persistent-soft" (persistent-soft :type git :flavor melpa :host github :repo "rolandwalker/persistent-soft") "pcache" (pcache :type git :flavor melpa :host github :repo "sigma/pcache") "list-utils" (list-utils :type git :flavor melpa :host github :repo "rolandwalker/list-utils") "ucs-utils" (ucs-utils :type git :flavor melpa :host github :repo "rolandwalker/ucs-utils") "almost-mono-themes" (almost-mono-themes :type git :flavor melpa :host github :repo "cryon/almost-mono-themes") "hide-mode-line" (hide-mode-line :type git :flavor melpa :host github :repo "hlissner/emacs-hide-mode-line") "dashboard" (dashboard :type git :flavor melpa :files (:defaults "banners" "dashboard-pkg.el") :host github :repo "emacs-dashboard/emacs-dashboard") "org-bullets" (org-bullets :type git :flavor melpa :host github :repo "integral-dw/org-bullets") "ido" nil "eshell" nil "olivetti" (olivetti :type git :flavor melpa :host github :repo "rnkn/olivetti") "org-roam" (org-roam :type git :flavor melpa :files (:defaults "extensions/*" "org-roam-pkg.el") :host github :repo "org-roam/org-roam") "emacsql" (emacsql :type git :flavor melpa :files ("emacsql.el" "emacsql-compiler.el" "emacsql-system.el" "README.md" "emacsql-pkg.el") :host github :repo "skeeto/emacsql") "emacsql-sqlite" (emacsql-sqlite :type git :flavor melpa :files ("emacsql-sqlite.el" "sqlite" "emacsql-sqlite-pkg.el") :host github :repo "skeeto/emacsql") "deft" (deft :type git :flavor melpa :host github :repo "jrblevin/deft") "mixed-pitch" (mixed-pitch :type git :flavor melpa :host gitlab :repo "jabranham/mixed-pitch") "emojify" (emojify :type git :flavor melpa :files (:defaults "data" "images" "emojify-pkg.el") :host github :repo "iqbalansari/emacs-emojify") "org-superstar" (org-superstar :type git :flavor melpa :host github :repo "integral-dw/org-superstar-mode") "whitespace" nil "mu4e" nil "gnus" nil "org-habit-plus" nil "org-dashboard" (org-dashboard :type git :flavor melpa :host github :repo "bard/org-dashboard") "borland-blue-theme" (borland-blue-theme :type git :flavor melpa :host github :repo "fourier/borland-blue-theme") "borland-theme" nil "base16-theme" (base16-theme :type git :flavor melpa :files (:defaults "build/*.el" "base16-theme-pkg.el") :host github :repo "belak/base16-emacs") "simple-theme" nil "basic-theme" (basic-theme :type git :flavor melpa :host github :repo "fgeller/basic-theme.el") "color-themes-sanityinc-tomorrow" nil "color-theme-sanityinc-tomorrow" (color-theme-sanityinc-tomorrow :type git :flavor melpa :host github :repo "purcell/color-theme-sanityinc-tomorrow") "rocket-chat" nil "promise" (promise :type git :flavor melpa :host github :repo "chuntaro/emacs-promise") "async-await" (async-await :type git :flavor melpa :host github :repo "chuntaro/emacs-async-await") "iter2" (iter2 :type git :flavor melpa :host github :repo "doublep/iter2") "ts-org-interaction" nil "shr" nil)) gnu-elpa-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 3 "emacsmirror-mirror" nil "straight" nil "nano-emacs" nil "nano-theme" (nano-theme :type git :host github :repo "emacs-straight/nano-theme" :files ("*" (:exclude ".git"))) "cl-lib" nil "persist" (persist :type git :host github :repo "emacs-straight/persist" :files ("*" (:exclude ".git"))) "ibuffer" nil "dired" nil "undo-tree" (undo-tree :type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git"))) "semantic" nil "json" nil "cc-mode" nil "perl" nil "seq" nil "align" nil "auctex" (auctex :type git :host github :repo "emacs-straight/auctex" :files ("*" (:exclude ".git"))) "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "flyspell" nil "project" (project :type git :host github :repo "emacs-straight/project" :files ("*" (:exclude ".git"))) "xref" (xref :type git :host github :repo "emacs-straight/xref" :files ("*" (:exclude ".git"))) "spinner" (spinner :type git :host github :repo "emacs-straight/spinner" :files ("*" (:exclude ".git"))) "exwm" (exwm :type git :host github :repo "emacs-straight/exwm" :files ("*" (:exclude ".git"))) "xelb" (xelb :type git :host github :repo "emacs-straight/xelb" :files ("*" (:exclude ".git"))) "cl-generic" nil "vertico" (vertico :type git :host github :repo "emacs-straight/vertico" :files ("*" (:exclude ".git"))) "savehist" nil "corfu" (corfu :type git :host github :repo "emacs-straight/corfu" :files ("*" (:exclude ".git"))) "ligature" nil "ido" nil "eshell" nil "whitespace" nil "mu4e" nil "gnus" nil "org-habit-plus" nil "borland-theme" nil "simple-theme" nil "color-themes-sanityinc-tomorrow" nil "rocket-chat" nil "ts-org-interaction" nil "shr" nil)) el-get #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 1 "emacsmirror-mirror" nil "straight" nil "nano-emacs" nil "cl-lib" nil "ibuffer" nil "dired" nil "semantic" nil "json" nil "cc-mode" nil "perl" nil "seq" nil "align" nil "flyspell" nil "cl-generic" nil "savehist" nil "ligature" nil "ido" nil "eshell" nil "whitespace" nil "mu4e" `(mu4e :type git :host github :repo "djcb/mu" :pre-build ,(cl-letf (((symbol-function #'el-get-package-directory) (lambda (package) (straight--repos-dir (format "%S" package)))) (el-get-install-info (straight--el-get-install-info)) (el-get-emacs (straight--el-get-emacs)) (el-get-dir (straight--repos-dir))) (pcase system-type (_ `(("./autogen.sh") ("make"))))) :files (:defaults "mu4e")) "gnus" nil "org-habit-plus" nil "borland-theme" nil "simple-theme" nil "color-themes-sanityinc-tomorrow" nil "rocket-chat" nil "ts-org-interaction" nil "shr" nil)) emacsmirror-mirror #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "nano-emacs" nil "cl-lib" nil "ibuffer" nil "dired" nil "semantic" nil "json" nil "cc-mode" nil "perl" nil "seq" nil "align" nil "flyspell" nil "cl-generic" nil "savehist" nil "ligature" nil "ido" nil "eshell" nil "whitespace" nil "gnus" nil "org-habit-plus" nil "borland-theme" nil "simple-theme" nil "color-themes-sanityinc-tomorrow" nil "rocket-chat" nil "ts-org-interaction" nil "shr" nil))))

("org-elpa" "melpa" "gnu-elpa-mirror" "el-get" "emacsmirror-mirror" "straight" "emacs" "use-package" "bind-key" "diminish" "selectrum" "consult" "marginalia" "vertico" "orderless" "savehist" "corfu" "eshell" "org" "org-superstar" "ibuffer" "undo-tree" "projectile" "magit" "dash" "git-commit" "transient" "with-editor" "magit-section" "tide" "s" "flycheck" "pkg-info" "epl" "cl-lib" "let-alist" "seq" "typescript-mode" "rustic" "rust-mode" "f" "markdown-mode" "project" "xref" "spinner" "xterm-color" "exwm" "xelb" "cl-generic" "ligature" "whitespace" "unicode-fonts" "font-utils" "persistent-soft" "pcache" "list-utils" "ucs-utils" "almost-mono-themes" "org-roam" "emacsql" "emacsql-sqlite" "deft" "org-dashboard" "gnus" "rocket-chat" "promise" "async-await" "iter2" "request" "shr")

t
