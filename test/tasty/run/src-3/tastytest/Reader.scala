package tastytest

trait Reader[Ctx] extends Monad[[X] =>> Ctx => X] {
  extension [A, B](r: Ctx => A) {
    def flatMap(f: A => Ctx => B): Ctx => B =
      ctx => f(r(ctx))(ctx)
  }
  def pure[A](x: A): Ctx => A =
    ctx => x
}
