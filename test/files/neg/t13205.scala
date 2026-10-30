// Distinct classes of the same name are unrelated, also seen through a self type
trait A { class C; type AC = C; def mkA: AC = new C }
trait B { class C; type BC = C; def useB(c: BC) = c.toString }
trait X { self: A with B =>
  def run = useB(mkA)
  implicitly[AC <:< BC]
  implicitly[AC =:= BC]
}
