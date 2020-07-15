package tastytest

import util.Try
import tastytest.IO._

sealed abstract class IO[A] { self =>

  final def unsafeRun(): A = self match { // not stacksafe
    case Succeed(a)     => a
    case Effect(f)      => f()
    case FlatMap(io, f) => f(io.unsafeRun()).unsafeRun()
    case Map(io, f)     => f(io.unsafeRun())
  }

  def map[B](f: A => B): IO[B]         = IO.Map(self, f)
  def flatMap[B](f: A => IO[B]): IO[B] = IO.FlatMap(self, f)
}

object IO {
  private final case class Succeed[A](a: A)                        extends IO[A]
  private final case class Effect[A](a: () => A)                   extends IO[A]
  private final case class FlatMap[A, B](io: IO[A], f: A => IO[B]) extends IO[B]
  private final case class Map[A, B](io: IO[A], f: A => B)         extends IO[B]

  def succeed[A](a: A): IO[A] = Succeed(a)
  def effect[A](a: => A): IO[A] = Effect(() => a)
}
