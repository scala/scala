object Test {
  def main(args: Array[String]): Unit = {
    val map =  Map(1 -> "a", 2 -> "b", 3 -> "c")
    assert(map.filterKeys(_ % 2 == 0).isInstanceOf[scala.collection.immutable.Map[_,_]])
  }
}
