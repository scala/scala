object Test {
  def main(args: Array[String]): Unit = {
    assert(intWrapper(5) == 5)
    assert(5 == intWrapper(5))
    assert(5 == (5: java.lang.Integer))
    assert((5: java.lang.Integer) == 5)
    assert((5: java.lang.Integer) == intWrapper(5))
    assert(intWrapper(5) == (5: java.lang.Integer))
  }
}
