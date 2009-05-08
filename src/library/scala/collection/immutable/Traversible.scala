package scala.collection.immutable

import generic._

/** A subtrait of Traversible in package collection which represents traversibles
 *  that cannot be mutated.
 *  !!! todo: revise equality
 *  @author  Matthias Zenger
 *  @autor   Martin Odersky
 *  @version 2.8
 */
trait Traversible[+A] extends collection.Traversible[A] with TraversibleTemplate[A, Traversible[A]] with Immutable { self =>
  override protected[this] def newBuilder = Traversible.newBuilder
  override def traversibleBuilder[B]: Builder[B, Traversible[B], Any] = Traversible.newBuilder[B]
}

/* A factory object for the trait `Traversible` */
object Traversible extends TraversibleFactory[Traversible] {
  type Coll = Traversible[_]
  implicit def builderFactory[A]: BuilderFactory[A, Traversible[A], Coll] = new BuilderFactory[A, Traversible[A], Coll] { def apply(from: Coll) = from.traversibleBuilder[A] }
  def newBuilder[A]: Builder[A, Traversible[A], Any] = new mutable.ListBuffer
}


