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

package scala.tools.nsc
package interpreter

import scala.tools.reflect.StdTags
import scala.reflect.runtime.{ universe => ru }

trait StdReplTags extends StdTags {
  lazy val tagOfStdReplVals = tagOfStaticClass[StdReplVals]
  lazy val tagOfIMain = tagOfStaticClass[IMain]
  lazy val tagOfRepl = tagOfStaticClass[Repl]
}

object StdReplTags extends StdTags with StdReplTags {
  val u: ru.type = ru
  val m = u.runtimeMirror(getClass.getClassLoader)
}
