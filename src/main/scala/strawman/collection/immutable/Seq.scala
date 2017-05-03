package strawman
package collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.IterableFactory

import scala.Any

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with collection.SeqLike[A, Seq]

object Seq extends IterableFactory[Seq] {
  def empty[A <: Any]: Seq[A] = List.empty[A]
  def newBuilder[A <: Any]: Builder[A, Seq[A]] = List.newBuilder[A]
  def fromIterable[E](it: collection.Iterable[E]): Seq[E] = List.fromIterable(it)
}
