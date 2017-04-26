package strawman
package collection.mutable

import collection.OrderedSetFactory

import scala.Ordering
import scala.Predef.???

trait TreeSet[A]
  extends SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]] {

}

object TreeSet extends OrderedSetFactory[TreeSet] {
  def empty[A : Ordering]: TreeSet[A] = ???
  def orderedFromIterable[E: Ordering](it: collection.Iterable[E]): TreeSet[E] = ???
}