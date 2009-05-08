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
 *  @autor   Martin Odersky
 *  @version 2.8
 */
trait Iterable[A] extends Traversable[A] with collection.Iterable[A] with IterableTemplate[A, Iterable[A]] { self =>
  override protected[this] def newBuilder = Iterable.newBuilder
  override def traversableBuilder[B]: Builder[B, Iterable[B], Any] = Iterable.newBuilder[B]
}

/* A factory object for the trait `Iterable` */
object Iterable extends TraversableFactory[Iterable] {
  type Coll = Iterable[_]
  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new BuilderFactory[A, Iterable[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, Iterable[A], Any] = new ArrayBuffer
}

