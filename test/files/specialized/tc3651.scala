



class Base[@specialized(Int) A](val a: A)

class Derived(override val a: Int) extends Base[Int](a)

object Test {
  def main(args: Array[String]) {
    val lk: Base[Int] = new Derived(10)
    lk.a
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}
