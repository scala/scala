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

package scala.tools.partest
package utils

/** Loads scala-partest.properties from the jar. */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory    = "scala-partest"
  protected def pickJarBasedOn  = classOf[nest.Runner]
  override lazy val isAvian = javaVmName.contains("Avian")
}
