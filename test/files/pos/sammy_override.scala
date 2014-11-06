trait IntConsumer {
  def consume(x: Int): Unit
}

object Test {
  def anyConsumer(x: Any): Unit = ???
  val f: IntConsumer = anyConsumer
}