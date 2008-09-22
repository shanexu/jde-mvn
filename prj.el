(jde-project-file-version "1.0")
(jde-set-variables
 '(jde-compile-option-command-line-args
   (quote ("-Xlint:all" "-Xlint:-serial"))))

(require 'jde-mvn)
(with-pom nil
  (jde-mvn-set-jde-variables :include-dependency-sources t))
