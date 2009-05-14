package scala.collection.immutable

import generic._

/** A subtrait of collection.Iterable which represents iterables
 *  that cannot be mutated.
 *
 *  @author  Matthias Zenger
 *  @autor   Martin Odersky
 *  @version 2.8
 */
trait Iterable[+A] extends Traversable[A]
                      with collection.Iterable[A]
                      with TraversableClass[A, Iterable]
                      with IterableTemplate[A, Iterable[A]] {
  override def companion: Companion[Iterable] = Iterable
}

/* A factory object for the trait `Iterable` */
object Iterable extends TraversableFactory[Iterable] {
  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Iterable[A]] = new mutable.ListBuffer
}

