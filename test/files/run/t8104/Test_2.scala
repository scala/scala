trait Generic[T] { type Repr }
object Generic {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }
  import scala.language.experimental.macros
  implicit def materializeGeneric[T, Repr]: Generic.Aux[T, Repr] = macro Macros.impl[T]
}

object Test extends App {
  case class C(x: Int, y: Int)

  import scala.reflect.runtime.universe._
  def reprify[T, Repr](x: T)(implicit generic: Generic.Aux[T, Repr], tag: WeakTypeTag[Repr]) = {
    println(tag)
    println(tag.tpe.typeSymbol.info)
  }
  reprify(C(40, 2))

  implicitly[Generic.Aux[C, (Int, Int)]]
}
