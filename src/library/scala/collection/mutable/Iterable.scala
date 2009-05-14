/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.mutable

import generic._

/** A subtrait of collection.Iterable which represents iterables
 *  that can be mutated.
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

/* A factory object for the trait `Iterable` */
object Iterable extends TraversableFactory[Iterable] {
  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Iterable[A]] = new ArrayBuffer
}

