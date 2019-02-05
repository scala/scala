import scala.language.higherKinds

/**
 */
@simulacrum trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
}

class TestSimulacrum {
  @simulacrum def no1 = ???
  /** 1 */ @simulacrum def yes1 = ???

  class ann extends scala.annotation.StaticAnnotation
  @ann def no2 = ???
  /** 2 */ @ann def yes2 = ???
}
