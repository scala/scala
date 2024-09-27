
//> using options -Werror -Wunused:patvars -Yvalidate-pos:typer

class C {
  val m = Map(
    "first" -> Map((true, 1), (false, 2), (true, 3)),
    "second" -> Map((true, 1), (false, 2), (true, 3))
  )
  def f =
    m.map { case (a, m1) =>
      for {
        (status, lag) <- m1 if status
      } yield (a, status, lag)
    }
  def g =
    for {
      (a, m1) <- m
      (status, lag) <- m1 if status
    } yield (a, status, lag)
  def leading =
    for {
      _ <- List("42")
      i = 1
      _ <- List("0", "27")(i)
    } yield ()
}
