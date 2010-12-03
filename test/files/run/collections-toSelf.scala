object Test {
  val map = Map(1 -> 2)
  val set = Set(1, 2)
  val seq = collection.immutable.Seq(1, 2)

  def main(args: Array[String]): Unit = {
    assert(map.toMap eq map)
    assert(set.toSet eq set)
    assert(seq.toSeq eq seq)
  }
}
