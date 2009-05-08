/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.mutable

import generic._

/** A subtrait of collection.Traversible which represents traversibles
 *  that can be mutated.
 *
 *  @autor   Martin Odersky
 *  @version 2.8
 */
trait Traversible[A] extends collection.Traversible[A] with TraversibleTemplate[A, Traversible[A]] with Mutable { self =>
  override protected[this] def newBuilder = Traversible.newBuilder
  override def traversibleBuilder[B]: Builder[B, Traversible[B], Any] = Traversible.newBuilder[B]
}

/* A factory object for the trait `Traversible` */
object Traversible extends TraversibleFactory[Traversible] {
  type Coll = Traversible[_]
  implicit def builderFactory[A]: BuilderFactory[A, Traversible[A], Coll] = new BuilderFactory[A, Traversible[A], Coll] { def apply(from: Coll) = from.traversibleBuilder[A] }
  def newBuilder[A]: Builder[A, Traversible[A], Any] = new ArrayBuffer
}


