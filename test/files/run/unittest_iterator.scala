// Some iterator grouped/sliding unit tests
object Test {
  def it = (1 to 10).iterator
  def assertThat[T](expectedLength: Int, expectedLast: Seq[T])(it: Iterator[Seq[T]]) {
    val xs = it.toList
    def fail(msg: String) = "assertion failed on %s: %s".format(xs, msg)
    assert(xs.size == expectedLength, fail("expected length " + expectedLength))
    assert(xs.last == expectedLast, fail("expected last " + expectedLast))
  }

  def main(args: Array[String]): Unit = {
    val itSum = it.toStream.sum
    for (i <- it) {
      // sum of the groups == sum of the original
      val thisSum = ((it grouped i) map (_.sum)).toStream.sum
      assert(thisSum == itSum, thisSum + " != " + itSum)
    }

    // grouped
    assertThat(4, List(10)) { it grouped 3 }
    assertThat(3, List(7, 8, 9)) { it grouped 3 withPartial false }
    assertThat(4, List(10, -1, -1)) { it grouped 3 withPadding -1 }

    // testing by-name padding
    val padIt = it
    assertThat(4, List(10, 1, 2)) { it grouped 3 withPadding padIt.next }

    // sliding
    assertThat(8, List(8, 9, 10)) { it sliding 3 }
    assertThat(3, (3 to 10).toList) { it sliding 8 }
    assertThat(2, List(9, 10)) { it.sliding(8, 8) }
    assertThat(1, (1 to 8).toList) { it.sliding(8, 8) withPartial false }
    assertThat(2, List(9, 10, -1, -1, -1)) { it.sliding(5, 8) withPadding -1 }
    assertThat(1, (1 to 5).toList) { it.sliding(5, 8) withPartial false }

    // larger step than window
    assertThat(5, List(9)) { it.sliding(1, 2) }
    assertThat(3, List(9, 10)) { it.sliding(2, 4) }

    // make sure it throws past the end
    val thrown = try {
      val it = List(1,2,3).sliding(2)
      it.next
      it.next
      it.next
      false
    }
    catch {
      case _: NoSuchElementException => true
    }
    assert(thrown)
  }
}
