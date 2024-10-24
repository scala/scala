// using options -Vprint:~delambdafy

case class StringValue(value: String) extends AnyVal

trait Foo[A] {
  def singleMethod(arg: A): StringValue
}

class R {
  val foo1: Foo[Int] = new Foo[Int] {
    override def singleMethod(arg: Int): StringValue = new StringValue(arg.toString)
  }
  val foo2: Foo[Int] = (arg: Int) => new StringValue(arg.toString)
  val foo3 = (arg: Int) => new StringValue(arg.toString)

  def run() = {
    assert(foo1.singleMethod(1).toString == "StringValue(1)")
    assert(foo2.singleMethod(1).toString == "StringValue(1)") // throws ClassCastException
    assert(foo3(1).toString == "StringValue(1)")
  }
}
object Test extends App {
  new R().run()
}
