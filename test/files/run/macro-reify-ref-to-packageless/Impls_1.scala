import scala.reflect.makro.{Context => Ctx}

object Impls {
  val `Answer to the Ultimate Question of Life, the Universe, and Everything` = 42
  def foo(c: Ctx) = c.reify { `Answer to the Ultimate Question of Life, the Universe, and Everything` }
}
