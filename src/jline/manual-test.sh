#!/usr/bin/env bash
#
# Apparently the jline bundled with sbt interferes with testing some
# changes: for instance after changing the keybindings I kept seeing
# failures until I realized what was happening and bypassed sbt, like this.

java -cp lib_managed/jar/com.novocode/junit-interface/junit-interface-0.5.jar:lib_managed/jar/junit/junit/junit-4.8.1.jar:lib_managed/jar/org.fusesource.jansi/jansi/jansi-1.4.jar:lib_managed/jar/org.scala-tools.testing/test-interface/test-interface-0.5.jar:target/scala-2.9.0.1/test-classes:target/scala-2.9.0.1/jline_2.9.0-1-2.10.0-SNAPSHOT.jar \
org.junit.runner.JUnitCore scala.tools.jline.console.EditLineTest
