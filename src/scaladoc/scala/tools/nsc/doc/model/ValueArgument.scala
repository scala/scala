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
package doc
package model


/** A value that is passed as an argument to a value parameter. */
trait ValueArgument {

  /** The parameter as argument to which this value is passed, if it is known. */
  def parameter: Option[ValueParam]

  /** The expression that calculates the value. */
  def value: TreeEntity

}
