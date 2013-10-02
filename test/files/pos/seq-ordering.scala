import Ordering.Implicits._

class A {
  import Predef.{ implicitly => ? }

  ?[Ordering[List[Int]]]
  ?[Ordering[IndexedSeq[(Int, String)]]]
  ?[Ordering[Seq[Seq[Int]]]]
}
