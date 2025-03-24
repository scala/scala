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
package collection


/** A default map which builds a default `immutable.Map` implementation for all
  * transformations.
  */
@deprecated("DefaultMap is no longer necessary; extend Map directly", "2.13.0")
trait DefaultMap[K, +V] extends Map[K, V]
