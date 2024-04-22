object example {
  final case class Toto[@specialized(Int) A] (private val actualToString: String, a: A) {
    @inline def description: String = actualToString
  }
  def toto[A](a: A): Toto[A] = Toto("", a)
}

object Test extends App {
  import example._

  println(s"Hello World! ${toto(1)}")
}
