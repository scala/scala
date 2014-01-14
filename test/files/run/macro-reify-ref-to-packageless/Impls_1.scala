import scala.reflect.macros.blackbox.Context

object Impls {
  val `Answer to the Ultimate Question of Life, the Universe, and Everything` = 42
  def foo(c: Context) = c.universe.reify { `Answer to the Ultimate Question of Life, the Universe, and Everything` }
}
