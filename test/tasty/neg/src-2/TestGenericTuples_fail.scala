package tastytest

object TestGenericTuples {
  val test1 = GenericTuples.bigtuple // error
  val test2 = GenericTuples.smalltuple // ok
  val test3 = GenericTuples.pair // error
  val test4 = GenericTuples.simpleTuple // error
  val test5 = GenericTuples.emptyTuple // ok
  val test6 = GenericTuples.consumeBigTuple((1, "")) // error
}
