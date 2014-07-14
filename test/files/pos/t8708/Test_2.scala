import scala.language.higherKinds

trait ClientTypes[M[+_]] {
  final type Context[+A] = EitherT[M, String, A]
  object Context {
    def apply[A](ca: M[String \/ A]): Context[A] = EitherT[M, String, A](ca)
  }

  final type StatefulContext[+A] = EitherT[Context, String, A]
  object StatefulContext {
    def apply[A](state: Context[String \/ A]): StatefulContext[A] = ???
  }
}
