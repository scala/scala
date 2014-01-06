class M[+A, +B]

object Test {
  implicit class EitherOps[A, B](self: Either[A, B]) {
    def disjunction: M[A, B] = null
  }

  def foo = {
    val l: Either[Int, Nothing] = Left[Int, Nothing](1)

    var ok = EitherOps(l).disjunction

    val runawayTypeVar = l.disjunction

    // reported bug:
    // found   : M[Int,B]; required: M[Int,Nothing]
    val assign: M[Int, Nothing] = runawayTypeVar

    // variations on the theme, all failed before similarly.
    val assign1: M[Int, Nothing] = {val temp = runawayTypeVar; temp}
    val assign2: M[Int, String] = runawayTypeVar
    val assign3: M[Int, Nothing] = {val temp = Left(1).disjunction; temp}
  }
}
