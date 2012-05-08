trait Base {
  type Rep[T]
}

trait Expressions {
  // constants/symbols (atomic)
  abstract class Exp[T]
  // ...
  case class Sym[T](n: Int) extends Exp[T]

  // operations (composite, defined in subtraits)
  abstract class Def[T]

  // additional members for managing encountered definitions
  def findOrCreateDefinition[T](rhs: Def[T]): Sym[T]
  implicit def toExp[T:Manifest](d: Def[T]): Exp[T] = findOrCreateDefinition(d)
}

trait BaseExp extends Base with Expressions {
  type Rep[T] = Exp[T]

  def findOrCreateDefinition[T](rhs: Def[T]): Sym[T] = null // stub
}

trait NumericOps extends Base {
  def plus[T](x: Rep[T], y: Rep[T]): Rep[T]
}

trait NumericOpsExp extends BaseExp {
  case class Plus[T:Numeric](x: Rep[T], y: Rep[T])
    extends Def[T]

  def plus[T: Numeric](x: Rep[T], y: Rep[T]): Rep[T] = Plus[T](x,y)

  // Possible solutions:
// def plus[T: Numeric: Manifest](x: Rep[T], y: Rep[T]): Rep[T] = Plus[T](x, y)
// def plus[T](x: Rep[T], y: Rep[T])(implicit num: Numeric[T], man: Manifest[T]): Rep[T] = Plus(x,y)

}
