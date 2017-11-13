package strawman
package collection
package decorators

class IterableDecorator[A](val `this`: Iterable[A]) extends AnyVal {

  def foldSomeLeft[B](z: B)(op: (B, A) => Option[B]): B =
    `this`.iterator().foldSomeLeft(z)(op)

}
