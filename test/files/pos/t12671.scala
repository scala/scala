
//> using options -Xsource:3

import scala.collection.{mutable, IterableOnce}
import scala.collection.immutable.{AbstractSet, Set, SetOps}

final case class Foo[-T](components: IndexedSeq[Int])

sealed trait FooTrie[T]
    extends AbstractSet[Foo[T]]
    with SetOps[Foo[T], Set, FooTrie[T]] {

  override def fromSpecific(
      coll: IterableOnce[Foo[T]]
  ): FooTrie[T] = {
    coll.iterator.foldLeft(empty)(_ incl _) // error here
  }

  override def newSpecificBuilder
      : mutable.Builder[Foo[T], FooTrie[T]] = ???

  override def incl(elem: Foo[T]): FooTrie[T] = ???

  override def empty = FooTrie.empty[T]
  //override def empty: FooTrie[T] = FooTrie.empty[T]
}

object FooTrie {
  def empty[T]: FooTrie[T] = ???
}
