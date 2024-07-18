//> using options -Werror -Wunused:synthetics
trait Context[A] { def m(a: A): A = a } // Context has a member, so warn if unused

object Example {
  def g[A: Context] = f
  def f = 42
}
