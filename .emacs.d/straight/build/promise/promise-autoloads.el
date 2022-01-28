;;; promise-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "promise" "promise.el" (0 0 0 0))
;;; Generated autoloads from promise.el

(autoload 'promise-chain "promise" "\
Extract PROMISE, BODY include then, catch, done and finally.

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

\(fn PROMISE &rest BODY)" nil t)

(function-put 'promise-chain 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise" '("promise")))

;;;***

;;;### (autoloads nil "promise-core" "promise-core.el" (0 0 0 0))
;;; Generated autoloads from promise-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-core" '("promise-")))

;;;***

;;;### (autoloads nil "promise-es6-extensions" "promise-es6-extensions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from promise-es6-extensions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-es6-extensions" '("promise-")))

;;;***

;;;### (autoloads nil "promise-rejection-tracking" "promise-rejection-tracking.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from promise-rejection-tracking.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-rejection-tracking" '("promise-")))

;;;***

;;;### (autoloads nil nil ("promise-done.el" "promise-finally.el")
;;;;;;  (0 0 0 0))

;;;***

(provide 'promise-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; promise-autoloads.el ends here
