// scalac: -Wunused:explicits -Werror
//
trait Context[A]
trait ExplicitsOnly {
  def i(implicit s: String) = 42
  def f[A](implicit ctx: Context[A]) = 42
  def g[A: Context] = 42

  def warn(x: Int) = 42
}
