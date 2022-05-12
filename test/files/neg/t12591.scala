// scalac: -Werror -Wunused:synthetics
trait Context[A]

object Example {
  def g[A: Context] = f
  def f = 42
}
