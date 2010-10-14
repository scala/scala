object Test {
  def main(args: Array[String]): Unit = {
    assert(collection.mutable.ArraySeq() == Nil)
    assert(collection.mutable.ArraySeq() == Seq())
    assert(Seq() == collection.mutable.ArraySeq())
    assert(Nil == collection.mutable.ArraySeq())
  }
}
