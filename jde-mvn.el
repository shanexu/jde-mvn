;;; jde-mvn.el --- Maven2 integration for JDEE
;;
;; Copyright (c) 2008 Espen Wiborg <espenhw@grumblesmurf.org>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;;

(require 'cl)
(require 'jde)

(require 'pom-parser)

(defgroup jde-mvn nil
  "JDE Maven 2"
  :group 'jde
  :prefix "jde-mvn-")

(defcustom jde-mvn-read-args nil
  "*Specify whether to prompt for additional arguments to pass to
mvn.  If non-nil, the jde-mvn-build command prompts you for the
additional arguments."
  :group 'jde-mvn
  :type 'boolean)

(defcustom jde-mvn-build-hook '(jde-compile-finish-kill-buffer
                                jde-compile-finish-refresh-speedbar
                                jde-compile-finish-update-class-info)
  "*List of hook functions run by `jde-mvn-build' (see `run-hooks'). Each
function should accept two arguments: the compilation buffer and a string
describing how the compilation finished"
  :group 'jde-mvn
  :type 'hook)

(defvar jde-mvn-interactive-args-history nil
  "History of arguments entered in the minibuffer.")

(defvar jde-mvn-interactive-goals-history nil
  "History of goals entered in the minibuffer.")

(defvar jde-mvn-default-goals 'install)
(make-variable-buffer-local 'jde-mvn-default-goals)

(defun* jde-mvn-build (&optional (goals jde-mvn-default-goals)
                                 (pom-file (pom-find-pom-file pom-file-name t)))
  "Run the mvn program specified by `pom-maven-command' on the
given POM, triggering the given goals.  If `jde-mvn-read-args' is
non-nil, read additional arguments for mvn from the minibuffer.
If given a prefix arg, read goals from the minibuffer.  If given
two prefix args (e.g. C-u C-u jde-mvn-build, read both goals and
pom-file from the minibuffer."
  (interactive
   (let ((prompt-for (cond ((null current-prefix-arg) nil)
                           ((and (consp current-prefix-arg)
                                 (eql (car current-prefix-arg) 16))
                            '(goals pom-file))
                           (t '(goals)))))
     (when prompt-for
       (list
        (if (memq 'goals prompt-for)
            (setq jde-mvn-default-goals
                  (read-from-minibuffer "Goals: " nil nil nil
                                        jde-mvn-interactive-goals-history))
          jde-mvn-default-goals)
        (if (memq 'pom-file prompt-for)
            (pom-prompt-for-pom-file)
          (pom-find-pom-file))))))
  (let ((compile-command
         (mapconcat #'identity
                    `(,pom-maven-command
                      "-B" "-N" "-f" ,pom-file
                      ,@(cond ((symbolp goals)
                               (list (symbol-name goals)))
                              ((consp goals)
                               (mapcar #'symbol-name goals))
                              (t (list goals)))
                      ,@(when jde-mvn-read-args
                          (list (read-from-minibuffer "Extra args: " nil nil nil jde-mvn-interactive-args-history))))
                    " "))
        process-connection-type)
    (save-some-buffers (not compilation-ask-about-save) nil)
    (setq compilation-finish-functions 
          (list #'(lambda (buf msg)
                    (run-hook-with-args 'jde-mvn-build-hook buf msg)
                    (setq compilation-finish-functions nil))))
    (compilation-start compile-command)))

(defun jde-mvn-find-failed-tests ()
  (interactive)
  (pop-to-buffer (compilation-find-buffer))
  (or (looking-at "  \\(\\w+\\)(\\([[:alnum:].]+\\))")
      (re-search-forward "Failed tests:" (point-max) t))
  (forward-line 1)
  (when (looking-at "  \\(\\w+\\)(\\([[:alnum:].]+\\))")
    (let ((class (match-string-no-properties 2))
          (method (match-string-no-properties 1)))
      (jde-find-class-source class t))))


;;; Hack alert!
;;; Insinuate jde-mvn-build into the custom definition of
;;; jde-build-function
(let ((type (cadr (get 'jde-build-function 'custom-type))))
  (unless (member '(const jde-mvn-build) type)
    (put 'jde-build-function 'custom-type
         (list 'list
               (append (butlast type)
                       (list '(const jde-mvn-build))
                       (last type))))))

(provide 'jde-mvn)
