final class TraversableOnceOps[+A](val collection: TraversableOnce[A]) extends AnyVal {
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (collection.isEmpty) None else Some(collection.reduceLeft[B](op))
}
// error: type arguments [B] do not conform to method reduceLeft's type parameter bounds [B >: A]
//            if (collection.isEmpty) None else Some(collection.reduceLeft[B](op))
//                                                                        ^

class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
  def baz[B >: A](x: B): List[B] = x :: xs
}
