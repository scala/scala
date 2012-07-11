// none of these should complain about exhaustivity
class Test {
  // It would fail on the following inputs: (_, false), (_, true)
  def x(in: (Int, Boolean)) = in match { case (i: Int, b: Boolean) => 3 }

  // There is no warning if the Int is ignored or bound without an explicit type:
  def y(in: (Int, Boolean)) = in match { case (_, b: Boolean) => 3 }

  // Keeping the explicit type for the Int but dropping the one for Boolean presents a spurious warning again:
  // It would fail on the following input: (_, _)
  def z(in: (Int, Boolean)) = in match { case (i: Int, b) => 3 }
}