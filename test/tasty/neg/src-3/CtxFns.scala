package tastytest

object CtxFns {

  abstract class Context:
    def puts[T](t: T): Unit

  type Contextual[T] = Context ?=> T

  def puts[T](t: T): Contextual[Unit] = summon[Context].puts(t)

  class CtxBox[F[X] <: X ?=> X] {
    def foo[T]: F[T] = ???
  }

}
