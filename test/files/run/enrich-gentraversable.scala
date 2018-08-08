import scala.tools.partest.Util.ArrayDeep
import scala.language.implicitConversions
import scala.language.postfixOps

object Test extends App {
  import scala.collection.generic.IsIterableLike
  import scala.collection.{BuildFrom, Iterable, IterableOps}

  def typed[T](t : => T): Unit = {}
  def testIterableOps = {
    class FilterMapImpl[A, Repr](r: Repr, it: IterableOps[A, Iterable, _]) {
      final def filterMap[B, That](f: A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That =
        bf.fromSpecific(r)(it.flatMap(f(_).toSeq))
    }
    implicit def filterMap[Repr, A](r: Repr)(implicit fr: IsIterableLike[Repr]): FilterMapImpl[fr.A, Repr] =
      new FilterMapImpl[fr.A, Repr](r, fr.conversion(r))

    val l = List(1, 2, 3, 4, 5)
    val fml = l.filterMap(i => if(i % 2 == 0) Some(i) else None)
    typed[List[Int]](fml)
    println(fml)

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
  }
  def testIterable = {
    class FilterMapImpl[A, Repr](r: Repr, it: IterableOps[A, Iterable, _]) {
      final def filterMap[B, That](f: A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That = {
        val b = bf.newBuilder(r)
        for(e <- it) f(e) foreach (b +=)
        b.result
      }
    }
    implicit def filterMap[Repr, A](r: Repr)(implicit fr: IsIterableLike[Repr]): FilterMapImpl[fr.A,Repr] =
      new FilterMapImpl[fr.A, Repr](r, fr.conversion(r))

    val l = List(1, 2, 3, 4, 5)
    val fml = l.filterMap(i => if(i % 2 == 0) Some(i) else None)
    typed[List[Int]](fml)
    println(fml)

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
  }

  testIterableOps
  testIterable
}
