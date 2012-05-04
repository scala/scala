object Main {
  implicit class C(val s: String) extends AnyVal
  implicit class C2(val s: String) extends AnyRef

  implicit case class Foo(i: Int)
}
