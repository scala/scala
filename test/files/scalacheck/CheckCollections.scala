import org.scalacheck.Properties
import org.scalacheck.Prop._

import scala.reflect.internal.util.Collections._

object Test extends Properties("reflect.internal.util.Collections") {
  def map2ConserveOld[A <: AnyRef, B](xs: List[A], ys: List[B])(f: (A, B) => A): List[A] =
    if (xs.isEmpty || ys.isEmpty) xs
    else {
      val x1 = f(xs.head, ys.head)
      val xs1 = map2Conserve(xs.tail, ys.tail)(f)
      if ((x1 eq xs.head) && (xs1 eq xs.tail)) xs
      else x1 :: xs1
    }

  val testfun: (String, Int) => String = { case(x, y) =>
    x.toLowerCase + y.toString
  }
  val testid: (String, Int) => String = { case (x, y) => x }

  val prop1_map2Conserve = forAll { (xs: List[String], ys: List[Int]) =>
    val res = map2Conserve(xs, ys)(testid)
    res eq xs
  }

  val prop2_map2Conserve = forAll { (xs: List[String], ys: List[Int]) =>
    map2Conserve(xs, ys)(testid)  == map2ConserveOld(xs, ys)(testid) &&
    map2Conserve(xs, ys)(testfun) == map2ConserveOld(xs, ys)(testfun)
  }

  def checkStackOverflow() {
    var xs: List[String] = Nil
    var ys: List[Int]    = Nil
    for (i <- 0 until 250000) {
        xs = "X" :: xs
        ys = 1   :: ys
    }
    map2Conserve(xs, ys){ case(x, y) => x.toLowerCase + y.toString }
  }


  val tests = List(
    ("map2Conserve(identity)",   prop1_map2Conserve),
    ("map2Conserve == old impl", prop2_map2Conserve)
  )

  checkStackOverflow()

  for {
    (label, prop) <- tests
  } property(label) = prop
}
