object Test {
  def main(args: Array[String]): Unit = {
    assert(5 == (5: java.lang.Integer))
    assert((5: java.lang.Integer) == 5)
  }
}
