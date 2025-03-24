/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interactive.tests.core

private[tests] trait Reporter {
  def println(msg: Any): Unit
}

/** Reporter that simply prints all messages in the standard output.*/
private[tests] object ConsoleReporter extends Reporter {
  def println(msg: Any): Unit = { Console.println(msg) }
}

/** Reporter that swallows all passed message. */
private[tests] object NullReporter extends Reporter {
  def println(msg: Any): Unit = {}
}
