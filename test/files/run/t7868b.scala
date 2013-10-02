object A {
  def unapply(n: Int): Option[Int] = Some(1)
}

object Test extends App {
  import reflect.runtime.universe._
  println(reify {
    val A(x) = (0: Short)
    x
  })
}
