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

package scala.tools.tasty

/** Representation of the `Attributes` section of a TASTy file,
 * with a minimal API for what is relevant for reading of signatures.
 */
sealed trait Attributes {
  def isJava: Boolean
}


object Attributes {
  private class ConcreteAttributes(val isJava: Boolean) extends Attributes

  val empty: Attributes = new ConcreteAttributes(isJava = false)
  val javaSource: Attributes = new ConcreteAttributes(isJava = true)
}
