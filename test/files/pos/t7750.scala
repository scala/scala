trait LazyCombiner[Elem, +To, Buff <: Growable[Elem] with Sizing]
trait Growable[T]
trait Sizing


object Test {
  null.isInstanceOf[LazyCombiner[_, _, _]] // issued an existential feature warning
}
