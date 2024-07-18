//> using options -Wunused:explicits -Werror
//
trait Context[A]
trait ExplicitsOnly {
  def i(implicit s: String) = answer
  def f[A](implicit ctx: Context[A]) = answer
  def g[A: Context] = answer

  def warn(x: Int) = answer
  def answer: Int = 42
}
