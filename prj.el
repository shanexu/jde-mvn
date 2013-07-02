(jde-project-file-version "1.0")
(jde-set-variables
 '(jde-build-function 'jde-mvn-build)
 '(jde-compile-option-command-line-args
   (quote ("-Xlint:all" "-Xlint:-serial"))))

(require 'jde-mvn)
(with-pom nil
  (jde-mvn-set-jde-variables :include-dependency-sources t))

;; Map C-c C-v C-t to run current test
(define-key jde-mode-map [remap jde-jdb-menu-debug-applet]
  'jde-mvn-build-run-test)

