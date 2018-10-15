
// scalac: -Xfatal-warnings -Ywarn-unused
class Test {
  def xsUnused = {
    val xs: List[Int] = List(0)

    for (refute@1 <- xs) {}
  }
  def f() = for (Some(i: Int) <- List(Some(42))) println(i)
}
