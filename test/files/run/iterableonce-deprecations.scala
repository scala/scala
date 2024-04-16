//> using options -deprecation
//
//
//
// collections-strawman issue #467
object Test {
  def main(args: Array[String]): Unit = {
    val i: IterableOnce[Int] = List(1, 2, 3, 4, 5)

    i.to(List)

    i.toBuffer
    i.toArray
    i.toList
    i.toSet
    i.toIterable
    i.toSeq
    i.toStream
    i.toVector

    val ti: IterableOnce[(Int, Int)] = List((1,2), (3,4), (5,6))

    ti.toMap
  }
}
