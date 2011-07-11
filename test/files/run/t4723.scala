


object Test {
  def main(args: Array[String]) {
    assert(Nil == collection.parallel.ParSeq())
    assert(collection.parallel.ParSeq() == Nil)
  }
}
