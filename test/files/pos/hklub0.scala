class GenericCompanion[+CC[X] <: Iterable[X]]
object Test {
  val a : GenericCompanion[scala.collection.immutable.Seq] = null
  val b : GenericCompanion[scala.collection.mutable.Seq] = null
  List(a, b) // immutable.this.List.apply[GenericCompanion[Seq]](Test.this.a, Test.this.b)
}
