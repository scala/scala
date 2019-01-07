import scala.language.higherKinds
object t9451 {
  implicit def impl[I[_]]: {
    type F[X] = { type Self = I[X] }
  } = new {
    type F[X] = { type Self = I[X] }
  }

  implicitly[{type F[X] = { type Self = Iterable[X] }}]
}
