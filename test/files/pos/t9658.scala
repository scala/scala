sealed trait G[T]
case object GI extends G[Int]

class C {
  def typerFail[T](rt: G[T]): T = rt match {
    case GI =>
      { case x => x } : PartialFunction[Any, Any] // comment this line, compiles.
      0 // found Int, required T
  }
}
