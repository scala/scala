// scalac: -Werror -Xlint:deprecation

class A {
  val value = 0
  lazy val lazyvalue = 2
  def nullary = 5
  def emptyArg() = 10
  def oneArg(x: String) = 15
}

class C extends A {
  override object value     // fail
}
