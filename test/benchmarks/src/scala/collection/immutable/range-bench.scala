package scala.collection.immutable
package benchmarks

object RangeTest {
  // not inlined any more, needs investigation
  // 
  // class XXS {
  //   private val array = Array.range(0, 100)
  //   def tst = { var sum = 0; for (i <- 0 until array.length) sum += array(i); sum }
  // }
  
  var x: Int = 0
  
  def foreachSum(max: Int): Int = {
    var sum = 0
    1 to max foreach (sum += _)
    sum
  }
  def whileSum(max: Int) = {
    var sum = 0
    var num = 1
    while (num <= max) {
      sum += num
      num += 1
    }
    sum
  }

  def show(max: Int, foreachNanos: Long, whileNanos: Long) {
    val winner = if (foreachNanos < whileNanos) "foreachSum" else "whileSum"
    val ratio = if (foreachNanos < whileNanos) foreachNanos.toDouble / whileNanos else whileNanos.toDouble / foreachNanos
    println("1 to %d:, %12s wins, %.3f:  foreach %.3f   while %.3f".format(
      max, winner, ratio, 
      foreachNanos.toDouble / 1000000L, 
      whileNanos.toDouble / 1000000L)
    )
  }
  
  def run(max: Int) = {
    val foreachFirst = util.Random.nextBoolean
    val t1 = System.nanoTime
    x = if (foreachFirst) foreachSum(max) else whileSum(max)
    val t2 = System.nanoTime
    x = if (foreachFirst) whileSum(max) else foreachSum(max)
    val t3 = System.nanoTime
    
    val foreachNanos = if (foreachFirst) t2 - t1 else t3 - t2
    val whileNanos = if (foreachFirst) t3 - t2 else t2 - t1
    show(max, foreachNanos, whileNanos)
  }

  def main(args: Array[String]): Unit = {
    var max = if (args.isEmpty) 100 else args(0).toInt
    while (max > 0) {    
      run(max)
      run(max)
      run(max)
      max += (max / 7)
    }
  }
}
