class SomeClass

object Test {
  def main(args: Array[String]): Unit = {
    val cls: Predef.Class[SomeClass] = classOf[SomeClass]
    Console.println(cls.getName())
  }
}
