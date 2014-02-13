trait Covariant[+A]
trait Contra[-A] { def accept(p: A): Unit }
trait Invariant[A] extends Covariant[A] with Contra[A]

trait T
case class Unravel[A](m: Contra[A], msg: A) extends T

object Test extends Covariant[Any] {
  def g(m: Contra[Any]): Unit = m accept 5
  def f(x: T): Unit = x match {
    case Unravel(m, msg) => g(m)
    case _               =>
  }
  def main(args: Array[String]) {
    f(Unravel[String](new Contra[String] { def accept(x: String) = x.length }, ""))
  }
}
// java.lang.ClassCastException: java.lang.Integer cannot be cast to java.lang.String
//   at Test$$anon$1.accept(a.scala:18)
//   at Test$.g(a.scala:13)
//   at Test$.f(a.scala:15)
//   at Test$.main(a.scala:18)
//   at Test.main(a.scala)
