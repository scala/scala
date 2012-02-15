case class Two[@specialized A, @specialized B](v: A, w: B);

// This is here to tell me if the behavior changes, not because
// the output is endorsed.
object Test {
  def main(args: Array[String]): Unit = {
    println(Two("Hello", 12).getClass().getName())
    println(Two(12, "Hello").getClass().getName())
    println(Two("Hello", "World").getClass().getName())
    println(Two(12, 12).getClass().getName())
  }
}
