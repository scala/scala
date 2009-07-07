/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.mutable

import generic._

/** <p>
 *    A subtrait of <a href="../Iterable.html" target="contentFrame">
 *    <code>collection.Iterable</code></a> which represents iterables
 *    that can be mutated.
 *  </p>
 *
 *  @author   Martin Odersky
 *  @version 2.8
 */
trait Iterable[A] extends Traversable[A]
                     with collection.Iterable[A]
                     with TraversableClass[A, Iterable]
                     with IterableTemplate[A, Iterable[A]] {
  override def companion: Companion[Iterable] = Iterable
}

/** <p>
 *    A factory object for the trait <a href="Iterable.html"
 *    target="contentFrame"><code>Iterable</code></a>.
 *  </p>
 *
 *  @author   Martin Odersky
 *  @version 2.8
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Iterable[A]] = new ArrayBuffer
}

