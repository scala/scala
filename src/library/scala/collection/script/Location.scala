/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package script

/** Class `Location` describes locations in messages implemented by
 *  class [[scala.collection.script.Message]].
 *
 *  @author  Matthias Zenger
 *  @since   2.8
 */

@deprecated("scripting is deprecated", "2.11.0")
sealed abstract class Location

@deprecated("scripting is deprecated", "2.11.0")
case object Start extends Location

@deprecated("scripting is deprecated", "2.11.0")
case object End extends Location

@deprecated("scripting is deprecated", "2.11.0")
case object NoLo extends Location

@deprecated("scripting is deprecated", "2.11.0")
case class Index(n: Int) extends Location
