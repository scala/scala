package scala.collection.immutable

import generic._

/** A subtrait of collection.Iterable which represents iterables
 *  that cannot be mutated.
 *
 *  @author  Matthias Zenger
 *  @autor   Martin Odersky
 *  @version 2.8
 */
trait Iterable[+A] extends Traversible[A] with collection.Iterable[A] with IterableTemplate[A, Iterable[A]] { self =>
  override protected[this] def newBuilder = Iterable.newBuilder
  override def traversibleBuilder[B]: Builder[B, Iterable[B], Any] = Iterable.newBuilder[B]
}

/* A factory object for the trait `Iterable` */
object Iterable extends TraversibleFactory[Iterable] {
  type Coll = Iterable[_]
  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new BuilderFactory[A, Iterable[A], Coll] { def apply(from: Coll) = from.traversibleBuilder[A] }
  def newBuilder[A]: Builder[A, Iterable[A], Any] = new mutable.ListBuffer
}

