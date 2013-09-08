object Bug_NoUnique {

  type TypeCon[Env] = (Int, Env=>Double)

  case class Wrap[E](parent:E) {}

  type Alias2[E] = Wrap[E]

  def wrap[E,A,Y](v : (A,E=>Y)) : (A,Alias2[E]=>Y) =
	throw new Error("Body here")

  def test(x : TypeCon[Wrap[Unit]]) : TypeCon[Unit] = wrap(x)
}

