class SomeClass

object Test {
  def main(args: Array[String]): Unit = {
    val cls: Class = classOf[SomeClass]
    Console.println(cls.getName())
  }
}
