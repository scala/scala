package strawman
package collection
package decorators

class IterableDecorator[A](val `this`: Iterable[A]) extends AnyVal {

  /**
    * Left to right fold that stops if the combination function `op`
    * returns `None`
    * @param z the start value
    * @param op the binary operator
    * @tparam B the result type of the binary operator
    * @return the result of inserting `op` between consecutive elements of the collection,
    *         going left to right with the start value `z` on the left, and stopping when
    *         all the elements have been traversed or earlier if the operator returns `None`
    */
  def foldSomeLeft[B](z: B)(op: (B, A) => Option[B]): B =
    `this`.iterator().foldSomeLeft(z)(op)

  /**
    * Right to left fold that can be interrupted before traversing the whole collection.
    * @param z the start value
    * @param op the operator
    * @tparam B the result type
    * @return the result of applying the operator between consecutive elements of the collection,
    *         going right to left, with the start value `z` on the right. The result of the application
    *         of the function `op` to each element drives the process: if it returns `Left(result)`,
    *         then `result` is returned without iterating further; if it returns `Right(f)`, the function
    *         `f` is applied to the previous result to produce the new result and the fold continues.
    */
  def lazyFoldRight[B](z: B)(op: A => Either[B, B => B]): B = `this`.iterator().lazyFoldRight(z)(op)

}
