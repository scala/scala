import scala.collection.generic.IsIterable
import scala.language.implicitConversions

object Test {
  class Ops[I] {
    def method: Unit = ()
  }

  implicit def ToOps[Repr, L, R](aCol: Repr)(implicit isIterable: IsIterable[Repr]{type A = (L, R)}): Ops[isIterable.type] =
    new Ops[isIterable.type]

  List(1 -> 2).method
}
