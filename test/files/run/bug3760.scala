object Test {
  def main(args: Array[String]): Unit = {
    {
      val it = Iterable(1,2).iterator
      val xs = it.toList

      assert(it.isEmpty)
    }

    {
      val it = Iterator(1, 2)
      val xs = it.toStream.toList

      assert(it.isEmpty)
    }
  }
}
