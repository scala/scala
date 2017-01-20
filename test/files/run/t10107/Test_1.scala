object Test {
  def main(args: Array[String]): Unit = {
    new B_1()
    val bi = new B_1.BInner
    new bi.ProblemClass()
  }
}
