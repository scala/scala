
trait X {
  def f0() = for (i <- 1 to 5 ; j = 2 * i ; if j > 4) yield j  // ok, all semis

  def f1() = for (i <- 1 to 5 if i > 3; j = 2 * i) yield j     // ok, all semis

  def f2() = for (i <- 1 to 5 ; j = 2 * i if j > 4) yield j    // no, that's confusing

  def f3() = for {
    i <- 1 to 5
    j = 2 * i
    k = 2 * j if k > 4      // no, that's super confusing
  } yield k
}
