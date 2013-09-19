import scala.language.implicitConversions
import scala.collection.generic.CanBuildFrom
import scala.math.Ordering
import collection.{TraversableLike, SeqLike}
import collection.immutable.BitSet

class QuickSort[Coll](a: Coll) {
  //should be able to sort only something with defined order (someting like a Seq)
  def quickSort[T](implicit ev0: Coll => SeqLike[T, Coll],
                   cbf: CanBuildFrom[Coll, T, Coll],
                   n: Ordering[T]): Coll = {
    quickSortAnything(ev0, cbf, n)
  }

  //we can even sort a Set, if we really want to
  def quickSortAnything[T](implicit ev0: Coll => TraversableLike[T, Coll],
                           cbf: CanBuildFrom[Coll, T, Coll],
                           n: Ordering[T]): Coll = {
    import n._
    if (a.size < 2) {
      a
    } else {
      // We pick the first value for the pivot.
      val pivot = a.head
      val (lower, tmp) = a.partition(_ < pivot)
      val (upper, same) = tmp.partition(_ > pivot)
      val b = cbf()
      b.sizeHint(a.size)
      b ++= new QuickSort(lower).quickSortAnything
      b ++= same
      b ++= new QuickSort(upper).quickSortAnything
      b.result
    }
  }
}

class FilterMap[Repr](a: Repr) {
  def filterMap[A, B, That](f: A => Option[B])(implicit ev0: Repr => TraversableLike[A, Repr],
                                               cbf: CanBuildFrom[Repr, B, That]): That = {
    a.flatMap(e => f(e).toSeq)
  }
}

class FilterMapFixed[A, Repr <% TraversableLike[A, Repr]](a: Repr) {
  def filterMap2[B, That](f: A => Option[B])(implicit cbf: CanBuildFrom[Repr, B, That]): That = {
    a.flatMap(e => f(e).toSeq)
  }
}

object MyEnhancements {
  implicit def toQS[Coll](a: Coll) = new QuickSort(a)
  implicit def toFM[Coll](a: Coll) = new FilterMap(a)
  implicit def toFM2[A, Repr <% TraversableLike[A, Repr]](a: Repr) = new FilterMapFixed(a)
}

object Test extends App {

  import MyEnhancements._

  println("qwe".quickSort)
  println(Array(2, 0).quickSort.toList)
  println(Seq(2, 0).quickSort)
  //not very useful to sort a set, but just as a demonstration
  println(BitSet(2, 0).quickSortAnything)

  //need to hint type inferencer,
  //probably will be able to overcome after https://issues.scala-lang.org/browse/SI-4699  and
  // related issues are  fixed (by moving ev0 parameter from filterMap to toFM), see toFM2
  println("qwe".filterMap((c: Char) => Some(c.toInt)))
  println("qwe".filterMap((c: Char) => Some(c)))
  println(Array(2, 0).filterMap((c: Int) => Some(c.toInt)).toList)
  println(Seq(2, 0).filterMap((c: Int) => if (c < 2) Some(c + "!") else None))
  def test(i:Int) = Option(i)
  println(BitSet(2,0).filterMap(test))

  println(toFM2("qwe").filterMap2(c => Some(c)))
  println(toFM2(Array(2, 0)).filterMap2(c => Some(c.toInt)).toList)
  //No implicit view available from java.lang.String => scala.collection.TraversableLike[A,java.lang.String]. :(
  //Not anymore :)
  println("qwe".filterMap2(c => Some(c)))
}
