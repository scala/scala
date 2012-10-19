object Test {
  def main(args: Array[String]) {
    val r = 'a' to 'z'
    for (i <- -2 to (r.length + 2)) {
      assert(r.take(i) == r.toList.take(i), (i, r.take(i)))
      assert(r.drop(i) == r.toList.drop(i), (i, r.drop(i)))
    }
    assert(1L to 10L contains 3)
    assert(1L to 10L contains 3L)
    assert(1L to 10L contains BigInt(3))
    assert(1L to 10L contains 3.0d)
    assert(1L to 10L contains (3: Short))
    assert(1L to 10L contains BigDecimal(3))
    assert(!(1L to 10L contains 3.00000001d))
  }
}
