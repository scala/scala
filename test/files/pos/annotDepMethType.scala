case class pc(calls: Any*) extends TypeConstraint

object Main {
  class C0 { def baz: String = "" }
  class C1 { def bar(c0: C0): String @pc(c0.baz) = c0.baz }
  def trans(c1: C1): String @pc(c1.bar(throw new Error())) = c1.bar(new C0)
}
