abstract class A {
  private def foo = List(1, 2)
}
trait B extends A {
  private def foo = List("a", "b")
  // However it compiles correctly if the type is given:
  // private def foo: List[String] = List("a", "b")
}
