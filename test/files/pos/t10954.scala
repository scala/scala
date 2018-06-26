object scalaz {
  trait Parallel
  trait @@[A, T] { type Self = A ; type Tag = T }
}
import scalaz._


trait Foo[F[_]]

class Bar[A]
object Bar {
  implicit def foo: Foo[({ type λ[α] = Bar[α] @@ Parallel })#λ] = ???
}

object Main {
  // import Bar.foo -- Bar is not in the implicit scope because it's not in the parts of `@@[Bar[α], Parallel]`
  implicitly[Foo[({ type λ[α] = Bar[α] @@ Parallel })#λ]]
}