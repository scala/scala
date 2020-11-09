package tastytest

object TestGenericTuples {
  val test1 = GenericTuples.bigtuple // error
  val test2 = GenericTuples.smalltuple // ok
  val test3 = GenericTuples.pair // error
  val test4 = GenericTuples.simpleTuple // error
  val test5 = GenericTuples.emptyTuple // ok
  val test6 = GenericTuples.consumeBigTuple((1, "")) // error
  val test7 = GenericTuples.tuple // error
  val test8 = new GenericTuples.ConsumeTuple(???) // error
  val test9 = new GenericTuples.ConsumeTupleGen[(Int, String)](???) // error
}
