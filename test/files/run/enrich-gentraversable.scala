import scala.tools.partest.Util.ArrayDeep
import scala.language.implicitConversions
import scala.language.postfixOps

object Test extends App {
  import scala.collection.generic.IsIterable
  import scala.collection.{BuildFrom, Iterable, IterableOps, View}
  import scala.collection.immutable.TreeMap

  def typed[T](t : => T): Unit = {}
  def testIterableOps = {
    class FilterMapImpl[A, Repr](r: Repr, it: IterableOps[A, Iterable, _]) {
      final def filterMap[B, That](f: A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That =
        bf.fromSpecific(r)(it.flatMap(f(_)))
    }
    implicit def filterMap[Repr](r: Repr)(implicit fr: IsIterable[Repr]): FilterMapImpl[fr.A, Repr] =
      new FilterMapImpl[fr.A, Repr](r, fr(r))

    val l = List(1, 2, 3, 4, 5)
    val fml = l.filterMap(i => if(i % 2 == 0) Some(i) else None)
    typed[List[Int]](fml)
    println(fml)

    val lv = l.view
    val fmlv = lv.filterMap(i => if (i % 2 == 0) Some(i) else None)
    typed[View[Int]](fmlv)
    println(fmlv.toList)

    val a = Array(1, 2, 3, 4, 5)
    val fma = a.filterMap(i => if(i % 2 == 0) Some(i) else None)
    typed[Array[Int]](fma)
    println(fma.deep)

    val s = "Hello World"
    val fms1 = s.filterMap(c => if(c >= 'A' && c <= 'Z') Some(c) else None)
    typed[String](fms1)
    println(fms1)

    val fms2 = s.filterMap(c =>if(c % 2 == 0) Some(c.toInt) else None)
    typed[IndexedSeq[Int]](fms2)
    println(fms2)

    val m = Map(1 -> "foo", 2 -> "bar")
    val fmm = m.filterMap { case (k, v) => if (k % 2 == 0) Some(v -> k) else None }
    typed[Map[String, Int]](fmm)
    println(fmm)

    val tm = TreeMap(1 -> "foo", 2 -> "bar")
    val tmm = tm.filterMap { case (k, v) => if (k % 2 == 0) Some(v -> k) else None }
    typed[TreeMap[String, Int]](tmm)
    println(tmm)

    val mv = m.view
    val fmmv = mv.filterMap { case (k, v) => if (k % 2 == 0) Some(v -> k) else None }
    typed[View[(String, Int)]](fmmv)
    println(fmmv.toMap)

  }

  testIterableOps
}
