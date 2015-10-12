/** benchmark for testing equality.
 *  Mix: == between non-numbers ith Object.equals as equality: 66%
 *          50% of these are tests where eq is true.
 *       == between boxed integers: 17%
 *       == between boxed characters: 5%
 *       == between boxed bytes: 5%
 *       == between boxed longs: 5%
 *       == between boxed shorts: < 1%
 *       == between boxed floats: < 1%
 *       == between boxed doubles: < 1%
 * In all cases 50% of the tests return true.
 */
object eqeq extends testing.Benchmark {

  def eqeqtest[T](creator: Int => T, n: Int): Int = {
    val elems = Array.tabulate[AnyRef](n)(i => creator(i % 2).asInstanceOf[AnyRef])

    var sum = 0
    var i = 0
    while (i < n) {
      var j = 0
      while (j < n) {
        if (elems(i) == elems(j)) sum += 1
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
    sum += eqeqtest(x => if (x == 0) obj1 else obj2, 2000)
    sum += eqeqtest(x => x, 1000)
    sum += eqeqtest(x => x.toChar, 550)
    sum += eqeqtest(x => x.toByte, 550)
    sum += eqeqtest(x => x.toLong, 550)
    sum += eqeqtest(x => x.toShort, 100)
    sum += eqeqtest(x => x.toFloat, 100)
    sum += eqeqtest(x => x.toDouble, 100)
    assert(sum == 2968750)
  }
}
