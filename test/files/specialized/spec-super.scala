
// see ticket #3651
object Test {
  def main(args: Array[String]) {
    val s = new Extended("s")
    println(s.foo) //works

    val i = new Extended(1)
    println(i.foo) //infinite loop with StackOverflowError

    println(runtime.BoxesRunTime.integerBoxCount)
  }
}

class Base[@specialized(Int) T](val t: T) {
  def foo() :T = t
}
class Extended [@specialized(Int) T](t: T) extends Base[T](t) {
  override def foo() :T = super.foo
}
