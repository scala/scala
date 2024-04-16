//> using options -Werror -Wunused
object Test extends App {
  for (x @ 1 <- List(1.0)) {
    assert(x.isInstanceOf[Double])
    assert(!x.isNaN)
  }
}
