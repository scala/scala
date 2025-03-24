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

package scala.tools.util

import scala.util.control.ControlThrowable

/** This class exists to replace existing calls to `System.exit` or `sys.exit`
  * in the compiler. It is recommended to avoid using this class where possible,
  * and instead use a design that does not require terminating the JVM.
  *
  * @param code the exit code
  */
final case class SystemExit(code: Int) extends ControlThrowable(s"exit code $code")
