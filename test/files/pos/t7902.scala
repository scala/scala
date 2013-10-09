import scala.language.higherKinds

object Bug {
  class Tag[W[M1[X1]]]

  def ofType[W[M2[X2]]]: Tag[W] = ???
  type InSeq  [M3[X3]] = Some[M3[Any]]

  // fail
  val x = ofType[InSeq]

  // okay
  val y: Any = ofType[InSeq]
  object T {
    val z = ofType[InSeq]
  }
}
