class C
object C {
  implicit def companion = new C
}

object Test {
  implicit val inScope1, inScope2 = new C
  implicitly[C]: Unit // C.companion was used; whereas the ambiguity should abort the implicit search.
  implicitly[C]       // ambiguity reported, rather than falling back to C.companion
}
