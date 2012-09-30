import scala.reflect.macros.{Context => Ctx}

object Impls {
  val `Answer to the Ultimate Question of Life, the Universe, and Everything` = 42
  def foo(c: Ctx) = c.universe.reify { `Answer to the Ultimate Question of Life, the Universe, and Everything` }
}
