object Test {
  def main(args: Array[String]): Unit = {
    assert(1 to 10 drop 10 isEmpty)
    assert(1 until 10 drop 9 isEmpty)
    assert(1 to 10 by 2 drop 5 isEmpty)

    assert((1 to 10 drop 9) == Seq(10))
    assert((1 until 10 drop 9) == Nil)

    assert(Stream(1 to 10).flatten.toList == Stream(1 until 11).flatten.toList)
  }
}
