/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.scalap

/** Loads decoder.properties from the jar. */
object Properties extends scala.util.PropertiesTrait
{
  protected def propCategory    = "decoder"
  protected def pickJarBasedOn  = classOf[Classfile]
}
