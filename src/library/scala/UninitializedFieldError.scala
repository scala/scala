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

/** This class implements errors which are thrown whenever a
 *  field is used before it has been initialized.
 *
 *  Such runtime checks are not emitted by default.
 *  They can be enabled by the `-Xcheckinit` compiler option.
 */
final case class UninitializedFieldError(msg: String) extends RuntimeException(msg) {
  def this(obj: Any) = this("" + obj)
}
