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

    assert(java.lang.Float.floatToIntBits(math.signum(0f)) == 0x0)
    assert(java.lang.Float.floatToIntBits(math.signum(-0f)) == 0x80000000)

    assert(java.lang.Double.doubleToLongBits(math.signum(0d)) == 0x0L)
    assert(java.lang.Double.doubleToLongBits(math.signum(-0d)) == 0x8000000000000000L)
  }
}
