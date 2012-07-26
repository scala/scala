class Foo {
  @volatile final var x=10
  override def toString = "" + x
}

object Test {
  def main(args: Array[String]): Unit = {
    println((new Foo))
  }
}
