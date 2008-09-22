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
;;; Dependencies
;;
;; This package depends on JDEE (obviously) and nxml-mode (not so
;; obviously).
;;
;; JDEE is available from http://jdee.sourceforge.net/
;;
;; nxml-mode is part of Emacs as of Emacs 23.  For earlier versions it
;; is available as part of many Linux distributions, otherwise you may
;; find it at http://www.thaiopensource.com/nxml-mode/.
;;
;;; Examples
;;
;; A typical prj.el now looks like this:
;;
;; (jde-project-file-version "1.0")
;; (jde-set-variables
;;  '(jde-compile-option-command-line-args
;;    (quote ("-Xlint:all" "-Xlint:-serial"))))
;;
;; (require 'jde-mvn)
;; (with-pom nil
;;   (jde-mvn-set-jde-variables :include-dependency-sources t))
;;
;; You may also want to ask Maven to download sources; see the
;; functions `jde-mvn-resolve-source-artifacts'.
;;
;; A function is provided to add dependencies to the POM;
;; `jde-mvn-pom-add-dependency' is your friend.
;;

(require 'cl)
(require 'jde)

(defgroup jde-mvn nil
  "JDE Maven 2"
  :group 'jde
  :prefix "jde-mvn-")

(defcustom jde-mvn-pom-file-name "pom.xml"
  "*Default name of a POM file."
  :type 'string
  :group 'jde-mvn)

(defcustom jde-mvn-command "mvn"
  "*The command to execute Maven 2.  Set this to the full path to
`mvn' if that command is not on your path, or if it's called something
funky on your system."
  :type 'string
  :group 'jde-mvn)

(defcustom jde-mvn-local-repository (expand-file-name "~/.m2/repository")
  "*The path to your local Maven repository."
  :type 'string
  :group 'jde-mvn)

(defun* jde-mvn-find-pom-file (&optional (pom-file-name jde-mvn-pom-file-name) noerror)
  "Find the next POM file upwards in the directory hierarchy.
If NOERROR is NIL, an error will be signalled if no POM file
could be found."
  (interactive)
  (let ((tag (gensym)))
    (catch tag
      (let ((pom (expand-file-name jde-mvn-pom-file-name)))
        (while (not (file-exists-p pom))
          (if (jde-root-dir-p (file-name-directory pom))
              (if noerror
                  (throw tag nil)
                (error "%s not found" (file-name-nondirectory pom)))
            (setq pom (expand-file-name (concat "../" (file-name-nondirectory pom))
                                        (file-name-directory pom)))))
        pom))))

(require 'jde-mvn-pom)
(require 'jde-mvn-build)

(provide 'jde-mvn)
