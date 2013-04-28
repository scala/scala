
import scala.language.{ existentials }

object Test {
  def f() = { case class Bar(x: Int); Bar }
  def g() = { case class Bar(x: Int); Bar(5) }
  def h() = { case object Bar ; Bar }

  val f1 = f()
  val g1 = g()
  val h1 = h()

  def m[T: Manifest](x: T) = println(manifest[T])

  def main(args: Array[String]): Unit = {
    m(f)
    m(g)
    m(h)
    m(f1)
    m(g1)
    m(h1)
  }
}

class A1[T] {
  class B1[U] {
    def f = { case class D(x: Int) extends A1[String] ; new D(5) }
  }
}
