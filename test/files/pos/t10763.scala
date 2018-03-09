class Test {
  def xsUnused = {
    val xs: List[Int] = List(0)

    for (refute@1 <- xs) {}
  }
}
