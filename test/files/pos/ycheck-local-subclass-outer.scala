//> using options -Ycheck:_ -Werror

// The anonymous subclass of the inner class `It` has an outer field of its own and inherits one.
// ExplicitOuter gave its outer field definition the overloaded symbol of both, which made -Ycheck
// report a missing getter and crash re-typing the definition after erasure.
class H {
  private[this] var x = 1
  abstract class It { def g = x }
  def it: It = new It { def h = x }
}
