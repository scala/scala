class A extends Cloneable {
  @inline final def foo = clone()
}

object Test {
  val x = new A
  def main(args: Array[String]) = x.foo
}
