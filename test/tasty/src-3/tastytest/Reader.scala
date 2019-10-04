package tastytest

trait Reader[Ctx] extends Monad[[X] =>> Ctx => X] {
  def (r: Ctx => A) flatMap[A, B](f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
