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
package generic

import scala.collection._

/** This trait implements a forwarder for iterable objects. It forwards
 *  all calls to a different iterable object, except for
 *
 *  - `toString`, `hashCode`, `equals`, `stringPrefix`
 *  - `newBuilder`, `view`
 *  - all calls creating a new iterable object of the same kind
 *
 *  The above methods are forwarded by subclass <a href="../IterableProxy.html"
 *  target="ContentFrame">`IterableProxy`</a>.
 *
 *  @author  Martin Odersky
 *  @since   2.8
 */
@deprecated("forwarding is inherently unreliable since it is not automated and methods can be forgotten", "2.11.0")
trait IterableForwarder[+A] extends Iterable[A] with TraversableForwarder[A] {

  /** The iterable object to which calls are forwarded */
  protected def underlying: Iterable[A]

  // Iterable delegates
  // Iterable methods could be printed by  cat IterableLike.scala | sed -n '/trait Iterable/,$ p' | egrep '^  (override )?def'

  override def iterator: Iterator[A] = underlying.iterator
  override def sameElements[B >: A](that: GenIterable[B]): Boolean = underlying.iterator.sameElements(that)
}
