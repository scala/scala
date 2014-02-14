trait Generic[T] { type Repr }
object Generic {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }
  import scala.language.experimental.macros
  implicit def materializeGeneric[T]: Generic[T] = macro Macros.impl[T]
}

object Test extends App {
  case class C(x: Int, y: Int)

  import scala.reflect.runtime.universe._
  def reprify[T, Repr](x: T)(implicit generic: Generic.Aux[T, Repr], tag: WeakTypeTag[Repr]) = println(tag)
  reprify(C(40, 2))

  // this is a compilation error at the moment as explained in SI-8104
  // because matchesPt in implicit search says that depoly(<type of materializeGeneric>) isn't a subtype of Generic.Aux[C, (Int, Int)]
  // which is rightfully so, because depoly only replaces type parameters, not type members with wildcard types
  // however in the future we might want to relax the matchesPt check, so this might start compiling
  // therefore, if you've broken this test, then you should be happy, because most likely you've just enabled an interesting use case!
  implicitly[Generic.Aux[C, (Int, Int)]]
}
