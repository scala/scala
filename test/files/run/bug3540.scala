object Test {
  def main(args: Array[String]): Unit = {
    assert(List.iterate(List(1,2,3), 4)(_.tail).last.isEmpty)
    assert(Stream.iterate(Stream(1,2,3), 4)(_.tail).last.isEmpty)
    assert(Array.iterate(Array(1,2,3), 4)(_.tail).last.isEmpty)
  }
}
