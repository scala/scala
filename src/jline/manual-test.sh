#!/usr/bin/env bash
#
# Apparently the jline bundled with sbt interferes with testing some
# changes: for instance after changing the keybindings I kept seeing
# failures until I realized what was happening and bypassed sbt, like this.
CP=lib_managed/jars/com.novocode/junit-interface/junit-interface-0.9.jar:lib_managed/jars/junit/junit-dep/junit-dep-4.8.2.jar:lib_managed/jars/org.fusesource.jansi/jansi/jansi-1.10.jar:lib_managed/jars/org.hamcrest/hamcrest-core/hamcrest-core-1.1.jar:lib_managed/jars/org.scala-tools.testing/test-interface/test-interface-0.5.jar:target/scala-2.10/test-classes:target/scala-2.10/jline_2.10-2.11.0-SNAPSHOT.min.jar

sbt proguard
java -cp $CP org.junit.runner.JUnitCore scala.tools.jline.console.EditLineTest
