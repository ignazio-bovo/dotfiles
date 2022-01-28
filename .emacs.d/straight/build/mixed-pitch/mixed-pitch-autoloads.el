;;; mixed-pitch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "mixed-pitch" "mixed-pitch.el" (0 0 0 0))
;;; Generated autoloads from mixed-pitch.el

(autoload 'mixed-pitch-mode "mixed-pitch" "\
Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.

If called interactively, enable Mixed-Pitch mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mixed-pitch" '("mixed-pitch-")))

;;;***

(provide 'mixed-pitch-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mixed-pitch-autoloads.el ends here
