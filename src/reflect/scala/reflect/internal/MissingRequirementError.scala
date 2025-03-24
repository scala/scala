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

package scala
package reflect
package internal

class MissingRequirementError private (msg: String) extends FatalError(msg) {
  import MissingRequirementError.suffix
  def req: String = if (msg endsWith suffix) msg dropRight suffix.length else msg
}

object MissingRequirementError {
  private val suffix = " not found."
  def signal(msg: String): Nothing = throw new MissingRequirementError(msg)
  def notFound(req: String): Nothing = signal(req + suffix)
  def unapply(x: Throwable): Option[String] = x match {
    case x: MissingRequirementError => Some(x.req)
    case _ => None
  }
}


