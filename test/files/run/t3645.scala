object Test {
  def main(args: Array[String]) {
    val s = Stream.tabulate(5)(x => x+2)
    assert( s.toList == List(2,3,4,5,6) )
  }
}
