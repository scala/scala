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

package scala.tools.testkit

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object CompileTime {
  def versionNumberString: String = macro versionNumberStringImpl
  def versionNumberStringImpl(c: blackbox.Context): c.Tree = {
    import c.universe._
    q"${scala.util.Properties.versionNumberString}"
  }
}
