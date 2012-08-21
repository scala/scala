class C(val a: Any) extends AnyVal
class A {
  implicit def c2AnyRef(c: C): AnyRef = new {}
  val c = new C(0)
  type t = c.type
}
