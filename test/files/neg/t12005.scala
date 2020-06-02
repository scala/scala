class A
class B
class MappedEncoding[I, O]

trait Encoding {
  type Encoder[T]
  implicit def mappedEncoder[I, O](implicit mapped: MappedEncoding[I, O], encoder: Encoder[O]): Encoder[I]
  implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], decoder: Encoder[I]): Encoder[O]
}

trait Tag[T]
object Tag {
  type Context[T] = Encoding with Tag[T]
}

trait WithContext[T] {
  protected val context: Tag.Context[T]
  implicit val encoder: MappedEncoding[A, B]
  implicit val decoder: MappedEncoding[B, A]
}

trait AbstractSaleRepository[T] extends WithContext[T] {
  import context._
  implicitly[Encoder[A]]
}
