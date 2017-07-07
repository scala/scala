package a {
  // A is a class, so W does not conform to A in bytecode. an access (w: W).m() requires a cast to A.
  // If `A` is not accessible, there's no solution.
  trait W extends A
  class C extends W
}

object Test {
  def main(args: Array[String]): Unit = {
    val w: a.W = new a.C
    w.m()
  }
}
