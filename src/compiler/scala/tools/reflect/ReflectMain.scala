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

package scala.tools
package reflect

import scala.reflect.internal.util.ScalaClassLoader
import scala.tools.nsc.{Driver, Global, CloseableRegistry, Settings}
import scala.tools.util.PathResolver

object ReflectMain extends Driver {

  private def classloaderFromSettings(settings: Settings) = {
    val classPathURLs = new PathResolver(settings, new CloseableRegistry).resultAsURLs
    ScalaClassLoader.fromURLs(classPathURLs, getClass.getClassLoader)
  }

  override def newCompiler(): Global = new ReflectGlobal(settings, reporter, classloaderFromSettings(settings))
}
