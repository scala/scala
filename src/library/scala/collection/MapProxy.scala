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

/** This is a simple wrapper class for [[scala.collection.Map]].
 *  It is most useful for assembling customized map abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @since   1
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.3")
trait MapProxy[A, +B] extends Map[A, B] with MapProxyLike[A, B, Map[A, B]]
