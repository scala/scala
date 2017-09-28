object Test {
  def main(args: Array[String]): Unit = {
    def partially: Any = List(1).collect { case _ => return "a" }
    def totally: Any = List(1).map { case _ => return "a" }
    assert( partially == "a" )
    assert( totally == "a" )
  }
}
