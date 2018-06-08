
object Test {
  trait T
  implicit def f(t: T): String = "I am the tee"
  //val n = "hello, world" length
  val g: Int => Int = 1 +
  def main(args: Array[String]): Unit = println {(
    null.asInstanceOf[T] : String,
    List(1).map(g),
  )}
}
