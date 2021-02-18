
import annotation.implicitNotFound
import scala.annotation.nowarn

// Ensure that an annotation doesn't break the message
@implicitNotFound("$foo": @nowarn)
trait Foo

// Ensure that a type ascription doesn't break the message
@implicitNotFound("bar": String)
trait Bar

object Example {
  implicitly[Foo]
  implicitly[Bar]
}
