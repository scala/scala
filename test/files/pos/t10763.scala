
//> using options -Werror -Wunused

class Test {
  def xsUnused = {
    val xs: List[Int] = List(0)

    for (refute@1 <- xs) println(refute)
  }
  def f() = for (Some(i: Int) <- List(Option(42))) println(i)
}
