object Test {
  def main(args: Array[String]) {
    assert(math.signum(Long.MaxValue) == 1L)
    assert(math.signum(1L) == 1L)
    assert(math.signum(0L) == 0L)
    assert(math.signum(-1L) == -1L)
    assert(math.signum(Long.MinValue) == -1L)

    assert(math.signum(Int.MaxValue) == 1)
    assert(math.signum(1) == 1)
    assert(math.signum(0) == 0)
    assert(math.signum(-1) == -1)
    assert(math.signum(Int.MinValue) == -1)
  }
}
