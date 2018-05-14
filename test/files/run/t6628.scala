object Test {
  def coll = new Iterable[String] {
    override def iterator: Iterator[String] = Iterator("1")
  }
  val dropped = coll.view drop 1

  def main(args: Array[String]): Unit = {
    println(dropped.isEmpty)
    println(dropped.toIndexedSeq.isEmpty)
  }
}
