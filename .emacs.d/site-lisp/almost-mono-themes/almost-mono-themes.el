;;; almost-mono-themes.el --- Almost monochromatic color themes -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 John Olsson

;; Author: John Olsson <john@cryon.se>
;; Maintainer: John Olsson <john@cryon.se>
;; URL: https://github.com/cryon/almost-mono-themes
;; Created: 9th May 2019
;; Version: 1.0.0
;; Keywords: faces
;; Package-Requires: ((emacs "24"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A suite of almost monochrome Emacs themes

;;; Code:

(defconst almost-mono-themes-colors
  '((white . ((background . "white")
          (foreground . "black")
          (weak       . "dim gray")
          (weaker     . "grey77")
          (weakest    . "gray94")
          (comment    . "grey54")
          (highlight  . "grey90")
          (highlight-fg  . "black")
          (warning    . "red")
          (keyword    . "black")
          (function    . "black")
          (variable    . "black")
          (builtin    . "black")
          (constant    . "black")
          (type    . "black")
          (alt-background    . "grey94")
          (preprocessor    . "aquamarine4")
          (success    . "aquamarine4")
          (string     . "aquamarine4")))

    (black . ((background . "black")
          (foreground . "white")
          (weak       . "yellow")
          (weaker     . "green")
          (comment    . "grey64")
          (weakest    . "cyan")
          (highlight  . "blue4")
          (highlight-fg  . "white")
          (warning    . "red")
          (success    . "gold")
          (keyword    . "yellow")
          (function    . "green")
          (variable    . "cyan1")
          (builtin    . "green")
          (constant    . "yellow")
          (alt-background    . "#000059")
          (type    . "cyan")
          (preprocessor    . "magenta")
          (string     . "magenta")))

    (blue . ((background . "#000059")
          (foreground . "white")
          (weak       . "yellow")
          (weaker     . "green")
          (comment    . "gray64")
          (weakest    . "cyan")
          (highlight  . "blue3")
          (highlight-fg  . "white")
          (warning    . "red")
          (success    . "gold")
          (keyword    . "yellow")
          (function    . "green")
          (variable    . "cyan1")
          (builtin    . "green")
          (constant    . "yellow")
          (alt-background    . "blue4")
          (type    . "cyan")
          (preprocessor    . "magenta")
          (string     . "magenta")))

    (gray .  ((background . "grey87")
          (foreground . "black")
          (weak       . "grey10")
          (weaker     . "grey18")
          (comment    . "grey30")
          (weakest    . "dark gray")
          (highlight  . "gray75")
          (highlight-fg  . "black")
          (warning    . "red2")
          (keyword    . "blue")
          (function    . "red")
          (alt-background    . "grey80")
          (variable    . "black")
          (builtin    . "red")
          (constant    . "dark green")
          (type    . "magenta2")
          (preprocessor    . "red")
          (success    . "saddle brown")
          (string     . "magenta2")))

    (cream . ((background . "#f0e5da")
          (foreground . "#000000")
          (weak       . "#7d7165")
          (weaker     . "#c4baaf")
          (comment    . "gray94")
          (weakest    . "#dbd0c5")
          (highlight  . "#fda50f")
          (highlight-fg  . "black")
          (warning    . "#ff0000")
          (keyword    . "yellow")
          (function    . "green")
          (variable    . "cyan1")
          (builtin    . "green")
          (alt-background    . "blue2")
          (constant    . "yellow")
          (type    . "cyan")
          (preprocessor    . "magenta")
          (success    . "#00ff00")
          (string     . "#3c5e2b")))))

(defmacro almost-mono-themes--variant-with-colors (variant &rest body)
  "Execute BODY in a scope where the different colors for given VARIANT is bound."
  `(let* ((colors (or (cdr (assoc ,variant almost-mono-themes-colors))
              (error "No such theme variant")))
      (background (cdr (assoc 'background colors)))
      (foreground (cdr (assoc 'foreground colors)))
      (weak           (cdr (assoc 'weak colors)))
      (weaker     (cdr (assoc 'weaker colors)))
      (weakest    (cdr (assoc 'weakest colors)))
      (highlight  (cdr (assoc 'highlight colors)))
      (highlight-fg  (cdr (assoc 'highlight-fg colors)))
      (keyword  (cdr (assoc 'keyword colors)))
      (function  (cdr (assoc 'function colors)))
      (variable  (cdr (assoc 'variable colors)))
      (builtin  (cdr (assoc 'builtin colors)))
      (constant  (cdr (assoc 'constant colors)))
      (alt-background  (cdr (assoc 'alt-background colors)))
      (type  (cdr (assoc 'type colors)))
      (comment  (cdr (assoc 'comment colors)))
      (preprocessor  (cdr (assoc 'preprocessor colors)))
      (warning    (cdr (assoc 'warning colors)))
      (success    (cdr (assoc 'success colors)))
      (string     (cdr (assoc 'string colors))))
     ,@body))

(defmacro almost-mono-themes--faces-spec ()
  "Provide the faces specification."
  (quote
   (mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(

      ;; default
      (default (:background ,background :foreground ,foreground))
      (fringe  (:background ,background))
      (region  (:background ,highlight  :foreground ,foreground))
      (show-paren-match (:background ,background :foreground ,success :bold t))
      (show-paren-mismatch (:background ,background :foreground ,warning :bold t))
      (minibuffer-prompt (:weight bold :foreground ,foreground))
      (isearch (:background ,weakest :foreground ,foreground :bold t))
      (lazy-highlight (:background ,weakest :foreground ,foreground))
      (link (:underline t))

      ;; mode line
      (mode-line (:box (:line-width -1 :color ,variable)
               :background ,alt-background :foreground ,foreground))

      (mode-line-inactive (:box (:line-width -1 :color ,comment)
                :background ,background :foreground ,comment))

      ;; font lock
      (font-lock-keyword-face (:bold t :foreground ,keyword))
      (font-lock-function-name-face (:foreground ,function :italic t :family "Monospace"))
      (font-lock-variable-name-face (:foreground ,variable))
      (font-lock-warning-face (:foreground ,foreground :underline (:color ,warning :style wave)))
      (font-lock-builtin-face (:bold t, :foreground ,builtin))
      (font-lock-constant-face (:bold t :foreground ,constant))
      (font-lock-type-face (:foreground ,type :underline t))
      (font-lock-preprocessor-face (:foreground ,comment))
      (font-lock-comment-face (:foreground ,comment :italic t :family "Monospace"))
      (font-lock-string-face (:foreground ,string))
      (font-lock-doc-face (:inherit font-lock-comment-face))
      (line-number (:foreground ,comment))
      (linum (:inherit line-number))
      (highlight (:background ,highlight))
      (lazy-highlight (:foreground ,type :bold t :extend t))
      (vertical-border (:foreground ,comment))

      ;; eshell
      (eshell-prompt (:foreground ,foreground :bold t))
      (eshell-ls-directory (:foreground ,foreground :bold t))
      (eshell-ls-archive (:inherit eshell-ls-unreadable))
      (eshell-ls-backup (:inherit eshell-ls-unreadable))
      (eshell-ls-clutter (:inherit eshell-ls-unreadable))
      (eshell-ls-executable (:inherit eshell-ls-unreadable))
      (eshell-ls-missing (:inherit eshell-ls-unreadable))
      (eshell-ls-product (:inherit eshell-ls-unreadable))
      (eshell-ls-readonly (:inherit eshell-ls-unreadable))
      (eshell-ls-special (:inherit eshell-ls-unreadable))
      (eshell-ls-symlink (:inherit eshell-ls-unreadable))

      ;; consult
      (consult-preview-line (:background ,alt-background))
      (consult-line-number-prefix (:foreground ,string))
      (consult-preview-cursor (:foreground ,string))

      ;; git gutter
      (git-gutter:modified (:background ,highlight :foreground ,highlight))
      (git-gutter:added (:background ,success :foreground ,success))
      (git-gutter:deleted (:background ,warning :foreground ,warning))

      ;; diff hl
      (diff-hl-change (:background ,highlight :foreground ,highlight))
      (diff-hl-insert (:background ,success :foreground ,success))
      (diff-hl-delete (:background ,warning :foreground ,warning))

      ;; hl line
      (hl-line (:background ,alt-background))
      (highlight-current-line-face (:inherit hl-line))

      ;; orderless
      (orderless-match-face-0 (:bold t :underline t :foreground ,keyword))
      (orderless-match-face-1 (:bold t :underline t :foreground ,function))
      (orderless-match-face-2 (:bold t :underline t :foreground ,variable))
      (orderless-match-face-3 (:bold t :underline t :foreground ,string))

      ;; selectrum
      (selectrum-current-candidate (:background ,highlight :foreground ,highlight-fg))
      (selectrum-group-title (:foreground ,string))

      ;; org mode
      (org-table (:foreground ,string))
      (org-drawer (:foreground ,function))
      (org-level-1 (:bold t :foreground ,keyword))
      (org-level-2 (:foreground ,function :bold t))
      (org-level-3 (:foreground ,variable))
      (org-level-4 (:foreground ,constant))
      (org-level-5 (:foreground ,string))
      (org-headline-done (:foreground ,comment :strike-through t))
      (org-headline-todo (:foreground ,keyword))
      (org-ellipsis (:foreground ,comment))
      (org-formula (:inherit org-ellipsis))
      (org-done (:foreground ,variable))
      (org-todo (:inherit org-done))
      (org-property-value (:foreground ,function))
      (org-link (:foreground ,string :underline t))
      (org-date (:foreground ,keyword :underline t))
      (org-agenda-current-time (:bold t))
      (org-agenda-date (:foreground ,foreground :bold t))
      (org-agenda-date-weekend (:inherit org-agenda-date))
      (org-agenda-date-today (:foreground ,string :bold t))
      (org-agenda-structure (:inherit org-level-1))
      (org-agenda-structure-secondary (:inherit org-agenda-structure))
      (org-agenda-date-weekend-today (:inherit org-agenda-date-today))
      (org-agenda-dimmed-todo (:inherit org-todo))
      (org-time-grid (:inherit org-property-value))
      (org-imminent-deadline (:foreground ,string :bold t))
      (org-warning (:bold t))
      (org-scheduled (:foreground ,foreground))
      (org-scheduled-today (:inherit org-scheduled))
      (org-scheduled-previously (:inherit org-scheduled))
      (org-agenda-done (:foreground ,weak :strike-through t))
      (org-date-selected (:background ,alt-background :foreground ,type))
      (org-upcoming-deadline (:foreground ,string))

      ;; whitespace mode
      (whitespace-empty (:foreground ,comment :background ,background))
      (whitespace-big-indent (:inherit whitespace-empty))
      (whitespace-space (:inherit whitespace-empty))
      (whitespace-indentation (:inherit whitespace-empty))
      (whitespace-line (:foreground ,comment))
      (whitespace-space-before-tab (:foreground ,string :underline t))
      (whitespace-trailing (:inherit whitespace-space-before-tab))

      ;; magit
      (magit-diff-added (:background ,alt-background :foreground ,function :extend t))
      (magit-diff-removed (:background ,alt-background :foreground ,string :extend t))
      (magit-diff-added-highlight (:background ,alt-background :foreground ,function :extend t))
      (magit-diff-removed-highlight (:background ,alt-background :foreground ,string :extend t))
      (diff-refine-added (:background ,alt-background :foreground ,function :bold t :extend t))
      (diff-refine-removed (:background ,alt-background :foreground ,string :bold t :extend t))
      (magit-blame-heading (:background ,highlight :foreground ,highlight-fg :extend t))
      (magit-section-highlight (:background ,highlight :foreground ,highlight-fg :extend t))
      (magit-section-heading (:foreground ,type :extend t))
      (magit-diff-context-highlight (:inherit default :extend t))
      (magit-blame-highlight (:inherit magit-blame-heading :extend t))
      (magit-diff-hunk-heading (:inherit magit-blame-heading :foreground ,variable :extend t))
      (magit-diff-revision-summary-highlight (:inherit magit-blame-heading :foreground ,type :extend t))
      (magit-diff-file-heading-highlight (:inherit magit-blame-heading :extend t))
      (magit-diff-hunk-heading-highlight(:inherit magit-blame-heading :foreground ,keyword :extend t)
      (magit-diff-revision-summary (:inherit magit-blame-heading :extend t))
      (magit-diff-revision-summary-highlight (:inherit magit-blame-heading :background ,alt-background :extend t))
      (magit-section-highlight (:inherit magit-blame-heading :extend t))
      (magit-blame-margin (:inherit magit-blame-heading :extend t))
      (magit-diff-file-heading-highlight (:inherit magit-blame-heading :extend t))

      ;; compilation
      (compilation-mode-line-exit (:foreground ,function :background ,background))
      ;; rocket-chat
      (rc-participant (:background ,background :foreground ,keyword :extend t))
      (rc-prompt (:background ,background :foreground ,function :bold t :extend t))
      (rc-user-offline (:background ,background :foreground ,comment :extend t))
      (rc-username (:background ,background :foreground ,string :extend t))

      ;; erc
      (erc-direct-message (:background ,background :foreground ,keyword))
      (erc-notice (:background ,background :foreground ,function))
      (erc-input (:background ,background :foreground ,variable))
      (erc-keyword (:background ,background :foreground ,type))

      ;; shr
      (shr-link (:foreground ,variable))
      (shr-selected-link (:background ,alt-background :foreground ,variable))
      )))))

(defun almost-mono-themes--variant-name (variant)
  "Create symbol for color theme variant VARIANT."
  (intern (format "almost-mono-%s" (symbol-name variant))))

(defmacro almost-mono-themes--define-theme (variant)
  "Define a theme for the almost-mono variant VARIANT."
  (let ((name (almost-mono-themes--variant-name variant))
        (doc (format "almost mono theme (%s version)" variant)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (almost-mono-themes--variant-with-colors
        ',variant
        (apply 'custom-theme-set-faces ',name
               (almost-mono-themes--faces-spec)))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide 'almost-mono-themes)

;;; almost-mono-themes.el ends here
