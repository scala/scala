//> using options -Werror -Ypatmat-exhaust-depth 10
//
// With `-Ypatmat-exhaust-depth 10`, a formula of more than 1000 conjuncts or disjuncts exceeds the analysis budget;
// the domain of `Color` does. A reachability query that is contradictory on its face (a `False` conjunct, or `p` and
// `!p` among at most ten conjuncts) is decided before the budget is checked, as `simplify` reduces it to `False`.
object Test {
  def wildcardFirst(c: Color): Int = c match { case _ => 0; case Color.K5 => 1 }
  def duplicate(c: Color): Int = c match { case Color.K5 => 0; case Color.K5 => 1 }
  def guardFalse(c: Color): Int = c match { case Color.K5 if false => 0; case Color.K5 => 1; case Color.K6 => 2 }
  def distinct(c: Color): Int = c match { case Color.K5 => 0; case Color.K6 => 1 }
}
