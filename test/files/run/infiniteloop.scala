/** Tests the optimiser (not to loop on 'reverse'). */

object Test extends App {
  def foo(): Unit = {
    val s3 = LazyList.range(1, 1000) //100000 (ticket #153: Stackoverflow)

    // ticket #153
    def powers(x: Int) = if ((x&(x-1)) == 0) Some(x) else None
    println(s3.flatMap(powers).reverse)
    println(s3.flatMap(powers).reverse.toList)
  }

  foo()
}
