import annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.{IterableFactory, StrictOptimizedIterableOps, mutable}
import scala.collection.immutable.{ListMap, ListSet}
import scala.collection.mutable.{AbstractSet, HashMap, HashSet, Set, SetOps}

// Stub of HashSet, but not final, so we can extend from it (in Test below)
class HS[A]
  extends AbstractSet[A]
    with SetOps[A, HS, HS[A]]
    with StrictOptimizedIterableOps[A, HS, HS[A]]
    with collection.IterableFactoryDefaults[A, HS]
    with Serializable {
  override def iterableFactory: IterableFactory[HS] = ???
  def get(elem: A): Option[A] = ???
  def contains(elem: A): Boolean = ???
  def addOne(elem: A): HS.this.type = ???
  def clear(): Unit = ???
  def iterator: Iterator[A] = ???
  def subtractOne(elem: A): HS.this.type = ???
}

object Test {
  class HashMapCollision1[A, +B](var hash: Int, var kvs: ListMap[A, B @uV]) extends HashMap[A, B @uV]
  class HashSetCollision1[A](var hash: Int, var ks: ListSet[A]) extends HS[A]

  def splitArray[T](ad: Array[Iterable[T]]): Any =
    ad(0) match {
      case _: HashMapCollision1[_, _] | _: HashSetCollision1[_] => null
    }

    // without type ascription for the one in the body of the last flatmap of each alternative, type inference borks on the existentials
    // def splitArray[T >: Nothing <: Any](ad: Array[Iterable[T]]): Any = { import OptionMatching._
    //   runOrElse(ad.apply(0))(((x1: Iterable[T]) => (
    //     or(((x4: Iterable[T]) => one(null)),
    //       guard(x1.isInstanceOf[Iterable[T] with Test.HashMapCollision1[_,_]], x1.asInstanceOf[Iterable[T] with Test.HashMapCollision1[_,_]]).flatMap(((x2: Iterable[T] with Test.HashMapCollision1[_,_]) => one(x2))),
    //       guard(x1.isInstanceOf[Test.HashSetCollision1[_]], x1.asInstanceOf[Iterable[T] with Test.HashSetCollision1[_]]).flatMap(((x3: Iterable[T] with Test.HashSetCollision1[_]) => one(x3)))): Option[Any]).orElse(
    //     (zero: Option[Any])))
    //   )
    // }

}

