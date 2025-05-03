object X { implicit def x: Int = 42 }

trait T { implicit def x: Int = 27 }

object Y extends T {
  import X._
  def f: Int = implicitly[Int]
}
