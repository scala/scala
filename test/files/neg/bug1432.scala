object Bug_NoUnique {

  type TypeCon[Env] = (Int, Env=>Double)

  case class Wrap[E](parent:E) {}

  def wrap[E,A,Y](v : (A,E=>Y)) : (A,Wrap[E]=>Y) =
	throw new Error("Body here")

  def test(x : TypeCon[Wrap[Unit]]) : TypeCon[Unit] = wrap(x)
}

