trait Matcher[AA]
case object MatchOne extends Matcher[Int]

object CollectIssue {
  def apply[A](m: Matcher[A]): A = m match {
    case MatchOne =>
      // This seems to break GADT refinement of A to Int.
      // Comment it out and the program typechecks.
      // Expanding the pattern matching anon partial function manually
      // also allows compilation.
      { case _ => 0 }: PartialFunction[Any, Int]

      // should conform to A, but doesn't.

      1
  }
}
