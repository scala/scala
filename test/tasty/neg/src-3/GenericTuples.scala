package tastytest

object GenericTuples {
  val bigtuple = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23) // *: erases to TupleXXL
  val smalltuple = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)  // statically Tuple22
  val pair: Int *: String *: EmptyTuple = (1,"hello")                          // *: erases to Product
  val simpleTuple: Int *: EmptyTuple = 1 *: EmptyTuple                         // *: erases to Product
  val emptyTuple: EmptyTuple = EmptyTuple                                      // statically EmptyTuple.type

  val tuple: Tuple = (1,2,3)

  def consumeBigTuple[T <: (Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any)](
    f: T
  ): Any = f(12) // this case tests type trees rather than types

  class ConsumeTupleGen[T <: Tuple](t: T)
  class ConsumeTuple(t: Tuple)

}
