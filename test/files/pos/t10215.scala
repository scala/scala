import language.higherKinds

class One[F[_]](val f: F[One[F]]) extends AnyVal
class Two[A]()
object One {
  val _ = new One[Two](new Two)
}