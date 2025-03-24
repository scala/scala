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

package scala.reflect.macros
package contexts

trait Infrastructure {
  self: Context =>

  def settings: List[String] = {
    val us = universe.settings
    import us._
    userSetSettings collectFirst { case x: MultiStringSetting if x.name == XmacroSettings.name => x.value } getOrElse Nil
  }

  def compilerSettings: List[String] = universe.settings.recreateArgs

  def classPath: List[java.net.URL] = global.classPath.asURLs.toList
}
