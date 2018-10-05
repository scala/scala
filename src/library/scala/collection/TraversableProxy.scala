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

// Methods could be printed by  cat TraversableLike.scala | egrep '^  (override )?def'


/** This trait implements a proxy for traversable objects. It forwards
 *  all calls to a different traversable object
 *
 *  @author  Martin Odersky
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.3")
trait TraversableProxy[+A] extends Traversable[A] with TraversableProxyLike[A, Traversable[A]]
