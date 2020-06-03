import annotation.unused

object Test {
  def main(args: Array[String]): Unit = {
    {
      val it = Iterable(1,2).iterator
      @unused val xs = it.toList

      assert(it.isEmpty)
    }

    {
      val it = Iterator(1, 2)
      @unused val xs = it.to(LazyList).toList

      assert(it.isEmpty)
    }
  }
}
