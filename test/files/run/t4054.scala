







object Test {
  def main(args: Array[String]) {
    val it = Iterator.from(1).map(n => n * n).scanLeft(0)(_+_)
    
    assert(it.next == 0)
    assert(it.next == 1)
    assert(it.next == 5)
    assert(it.next == 14)
    assert(it.next == 30)
    assert(it.next == 55)
    assert(it.next == 91)
    assert(it.next == 140)
    assert(it.next == 204)
    assert(it.next == 285)
    assert(it.next == 385)
  }
}
