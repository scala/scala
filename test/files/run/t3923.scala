object Test {
  def main(args: Array[String]): Unit = {
    assert(collection.mutable.WrappedArray() == Nil)
    assert(collection.mutable.WrappedArray() == Seq())
    assert(Seq() == collection.mutable.WrappedArray())
    assert(Nil == collection.mutable.WrappedArray())
  }
}
