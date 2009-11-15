object eq extends testing.Benchmark {

  def eqtest[T](creator: Int => T, n: Int): Int = {
    val elems = Array.tabulate[AnyRef](n)(i => creator(i % 2).asInstanceOf[AnyRef])

    var sum = 0
    var i = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        if (elems(i) eq elems(j)) sum += 1
        j += 1
      }
      i += 1
    }
    sum
  }

  val obj1 = new Object
  val obj2 = new Object

  def run() {
    var sum = 0
    sum += eqtest(x => if (x == 0) obj1 else obj2, 2000)
    sum += eqtest(x => x, 1000)
    sum += eqtest(x => x.toChar, 550)
    sum += eqtest(x => x.toByte, 550)
    sum += eqtest(x => x.toLong, 550)
    sum += eqtest(x => x.toShort, 100)
    sum += eqtest(x => x.toFloat, 100)
    sum += eqtest(x => x.toDouble, 100)
    assert(sum == 2958950)
  }
}
