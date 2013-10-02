import scala.language.implicitConversions
import scala.language.postfixOps

object Test extends App {
  import scala.collection.{GenTraversableOnce,GenTraversableLike}
  import scala.collection.generic._

  def typed[T](t : => T) {}
  def testTraversableLike = {
    class FilterMapImpl[A, Repr](val r: GenTraversableLike[A, Repr]) /* extends AnyVal */ {
      final def filterMap[B, That](f: A => Option[B])(implicit cbf: CanBuildFrom[Repr, B, That]): That =
        r.flatMap(f(_).toSeq)
    }
    implicit def filterMap[Repr, A](r: Repr)(implicit fr: IsTraversableLike[Repr]): FilterMapImpl[fr.A,Repr] =
      new FilterMapImpl[fr.A, Repr](fr.conversion(r))

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
  def testTraversableOnce = {
    class FilterMapImpl[A, Repr](val r: GenTraversableOnce[A]) /* extends AnyVal */ {
      final def filterMap[B, That](f: A => Option[B])(implicit cbf: CanBuildFrom[Repr, B, That]): That = {
        val b = cbf()
        for(e <- r.seq) f(e) foreach (b +=)

        b.result
      }
    }
    implicit def filterMap[Repr, A](r: Repr)(implicit fr: IsTraversableOnce[Repr]): FilterMapImpl[fr.A,Repr] =
      new FilterMapImpl[fr.A, Repr](fr.conversion(r))

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

  testTraversableLike
  testTraversableOnce
}
