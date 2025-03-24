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

import settings.{DefaultPathFactory, MutableSettings, PathFactory}

/** A compatibility stub.
 */
class Settings(errorFn: String => Unit, pathFactory: PathFactory) extends MutableSettings(errorFn, pathFactory) {
  def this(errorFn: String => Unit) = this(errorFn, DefaultPathFactory)
  def this() = this(Console.println)

  override def withErrorFn(errorFn: String => Unit): Settings = {
    val settings = new Settings(errorFn)
    copyInto(settings)
    settings
  }
}
