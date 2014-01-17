class A {
  def f1 = "f1" isInstanceOf[String] // not ok
  def f2 = "f2".isInstanceOf[String] // ok
  def f3 = "f3" toList               // ok
}
