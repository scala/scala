import collection.immutable.SortedMap

object Test {
  implicit val ord: Ordering[Array[Byte]] = Ordering.by((_: Array[Byte]).toIterable)

  def main(args: Array[String]): Unit = {
    val m = SortedMap(Array[Byte](1) -> 0)
    println(m.to(Array[Byte](1)) flatMap (_._1.mkString))
    println(m.from(Array[Byte](1)) flatMap (_._1.mkString))
  }
}
