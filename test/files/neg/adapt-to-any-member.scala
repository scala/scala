
//> using options -Werror -Xlint:universal-methods

class C {
  import C._
  def f[A](x: A) = if (x eq null) 0 else 1 // warn
  def g[A](x: A) = if (x.hashCode(0) == 0) 0 else 1 // nowarn
}
object C {
  implicit class Elvis[A](alt: => A) {
    def ?:(a: A): A = if (a.asInstanceOf[AnyRef] ne null) a else alt
    def hashCode(seed: Int): Int = seed
  }
}
