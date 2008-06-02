(jde-project-file-version "1.0")

(require 'pom-parser)
(with-pom ()
  (pom-set-jde-variables :include-dependency-sources t))
