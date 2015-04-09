

case class C(s: String, xs: Int*)

object X { def unapplySeq(a: Any): Option[(String, Seq[Int])] = Some("", List(1,2,3)) }

// for case classes with varargs, avoid misaligned patterns
trait Ctest {
  def f = C("") match { case C(s) => }

  def g = C("") match { case C(s, t) => }

  def h = C("") match { case C(s, t, u @ _*) => }

  def ok = C("") match { case C(s, u @ _*) => }
}
// for extractors that unapplySeq: Option[(Something, Seq[_])], avoid misaligned patterns
trait Xtest {
  def f = "" match { case X(s) => }

  def g = "" match { case X(s, t) => }

  def h = "" match { case X(s, t, u @ _*) => }

  def ok = "" match { case X(s, u @ _*) => }
}
// for extractors that unapplySeq: Option[Seq[_]], anything goes
trait Rtest {
  val r = "(a+)".r

  def f = "" match { case r(s) => }

  def g = "" match { case r(s, t) => }

  def h = "" match { case r(s, t, u @ _*) => }

  def whatever = "" match { case r(u @ _*) => }
}
