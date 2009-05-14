package scala.collection.immutable

import generic._

/** A subtrait of Traversable in package collection which represents traversables
 *  that cannot be mutated.
 *  !!! todo: revise equality
 *  @author  Matthias Zenger
 *  @author   Martin Odersky
 *  @version 2.8
 */
trait Traversable[+A] extends collection.Traversable[A]
                         with TraversableClass[A, Traversable]
                         with TraversableTemplate[A, Traversable[A]]
                         with Immutable {
  override def companion: Companion[Traversable] = Traversable
}

/* A factory object for the trait `Traversable` */
object Traversable extends TraversableFactory[Traversable] {
  implicit def builderFactory[A]: BuilderFactory[A, Traversable[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Traversable[A]] = new mutable.ListBuffer
}


