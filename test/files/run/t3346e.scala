import scala.language.implicitConversions
import scala.math.Ordering
import collection.{BuildFrom, Iterable, IterableOps, SeqOps}
import collection.immutable.BitSet

class QuickSort[Coll](a: Coll) {
  //should be able to sort only something with defined order (someting like a Seq)
  def quickSort[T](implicit ev0: Coll => SeqOps[T, Any, Iterable[T]],
                   bf: BuildFrom[Coll, T, Coll],
                   n: Ordering[T]): Coll = {
    quickSortAnything(ev0, bf, n)
  }

  //we can even sort a Set, if we really want to
  def quickSortAnything[T](implicit ev0: Coll => IterableOps[T, Any, Iterable[T]],
                           bf: BuildFrom[Coll, T, Coll],
                           n: Ordering[T]): Coll = {
    import n._
    if (a.size < 2) {
      a
    } else {
      // We pick the first value for the pivot.
      val pivot = a.head
      val (lower, tmp) = a.partition(_ < pivot)
      val (upper, same) = tmp.partition(_ > pivot)
      val b = bf.newBuilder(a)
      b.sizeHint(a.size)
      b ++= new QuickSort(lower).quickSortAnything
      b ++= same
      b ++= new QuickSort(upper).quickSortAnything
      b.result
    }
  }
}

class FilterMap[Repr](a: Repr) {
  def filterMap[A, B, That](f: A => Option[B])(implicit ev0: Repr => IterableOps[A, Iterable, _],
                                               bf: BuildFrom[Repr, B, That]): That = {
    bf.fromSpecific(a)(a.flatMap(e => f(e)))
  }
}

class FilterMapFixed[A, Repr <% IterableOps[A, Iterable, _]](a: Repr) {
  def filterMap2[B, That](f: A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That = {
    bf.fromSpecific(a)(a.flatMap(e => f(e)))
  }
}

object MyEnhancements {
  implicit def toQS[Coll](a: Coll) = new QuickSort(a)
  implicit def toFM[Coll](a: Coll) = new FilterMap(a)
  implicit def toFM2[A, Repr <% IterableOps[A, Iterable, _]](a: Repr) = new FilterMapFixed(a)
}

object Test extends App {

  import MyEnhancements._

  println("qwe".quickSort)
  println(Array(2, 0).quickSort.toList)
  println(Seq(2, 0).quickSort)
  //not very useful to sort a set, but just as a demonstration
  println(BitSet(2, 0).quickSortAnything)

  //need to hint type inferencer,
  //probably will be able to overcome after https://github.com/scala/bug/issues/4699  and
  // related issues are  fixed (by moving ev0 parameter from filterMap to toFM), see toFM2
  println("qwe".filterMap((c: Char) => Some(c.toInt)))
  println("qwe".filterMap((c: Char) => Some(c)))
  println(Array(2, 0).filterMap((c: Int) => Some(c.toInt)).toList)
  println(Seq(2, 0).filterMap((c: Int) => if (c < 2) Some(s"$c!") else None))
  def test(i:Int) = Option(i)
  println(BitSet(2,0).filterMap(test))

  println(toFM2("qwe").filterMap2(c => Some(c)))
  println(toFM2(Array(2, 0)).filterMap2(c => Some(c.toInt)).toList)
  //No implicit view available from java.lang.String => scala.collection.TraversableLike[A,java.lang.String]. :(
  //Not anymore :)
  println("qwe".filterMap2(c => Some(c)))
}
