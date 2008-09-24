This package provides Maven 2 integration for JDEE, in the form of a
plugin.

Building from source
--------------------
Read BUILD.txt.

Installation
--------------------
Grab the jde-mvn-${version}-dist.zip file and unzip it in your JDEE
plugins directory (the value of jde-plugins-directory).  Optionally,
byte compile the Emacs Lisp files (in mvn/lisp).

Then either load JDEE or, if you already have it running, M-x
jde-pi-load-plugins.

You may now add 

(with-pom nil
  (jde-mvn-set-jde-variables :include-dependency-sources t))

to your project files.

In addition, jde-mvn-build is now a valid value of jde-build-function;
this function is also available in the JDEpi menu.

There are some tweakable settings; M-x customize-group jde-mvn for the
full list.
