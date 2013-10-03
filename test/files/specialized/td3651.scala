



class Base[@specialized(Double) A](val a: A)

class Derived(override val a: Double) extends Base[Double](a)

object Test {
  def main(args: Array[String]) {
    val b: Base[Double] = new Derived(10)
    b.a
    println(runtime.BoxesRunTime.doubleBoxCount)

    val der = new Derived(10)
    der.a
    println(runtime.BoxesRunTime.doubleBoxCount)
  }
}
