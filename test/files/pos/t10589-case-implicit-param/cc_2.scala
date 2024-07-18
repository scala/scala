//> using options -Ymacro-annotations
trait T[A]

@macid
case class CC[A: T](x: A)

