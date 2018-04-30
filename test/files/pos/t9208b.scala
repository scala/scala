
object X { implicit def x: Int = 42 }

trait T { implicit def x: Int = 17 }

object Y extends T {
  import X._
  def f: Int = implicitly[Int]
}

class C[A]()(implicit val ordering: Ordering[A]) {
  class D() extends C[A] {
    def f = implicitly[Ordering[A]]
  }
}
