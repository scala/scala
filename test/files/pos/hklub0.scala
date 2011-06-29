object Test {
  val a : scala.collection.generic.GenericCompanion[scala.collection.immutable.Seq] = null
  val b : scala.collection.generic.GenericCompanion[scala.collection.mutable.Seq] = null
  List(a, b) // immutable.this.List.apply[scala.collection.generic.GenericCompanion[Seq]](Test.this.a, Test.this.b)
}