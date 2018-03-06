package strawman.collection
package decorators

/**
  * @param coll the decorated collection
  * @param seq evidence that type `C` is a sequence
  * @tparam C type of the decorated collection (e.g. `List[Int]`, `String`, etc.)
  */
class SeqDecorator[C, S <: HasSeqOps[C]](coll: C)(implicit val seq: S) {

  /** Adds the element `sep` between each element of the sequence.
    * If the sequence has less than two elements, the collection is unchanged.
    *
    * @param sep the element to intersperse
    * @tparam B the element type of the returned collection
    * @return a new collection consisting of all elements of this collection
    *         interspersed with the element `sep`
    *
    * @example {{{
    * List(1, 2, 3, 4).intersperse(0) = List(1, 0, 2, 0, 3, 0, 4)
    * }}}
    */
  def intersperse[B >: seq.A, That](sep: B)(implicit bf: BuildFrom[C, B, That]): That =
    bf.fromSpecificIterable(coll)(new View.Intersperse(seq(coll), sep))

  /** Adds the element `sep` between each element of the sequence,
    * prepending `start` and appending `end`.
    * If the sequence has less than two elements, returns `start +: this :+ end`.
    *
    * @param start the element to prepend
    * @param sep the element to intersperse
    * @param end the element to append
    * @tparam B the element type of the returned collection
    * @return a new collection consisting of all elements of this collection
    *         interspersed with the element `sep`, beginning with `start` and ending with `end`
    *
    * @example {{{
    *      List(1, 2, 3, 4).intersperse(-1, 0, 5) => List(-1, 1, 0, 2, 0, 3, 0, 4, 5)
    * }}}
    */
  def intersperse[B >: seq.A, That](start: B, sep: B, end: B)(implicit bf: BuildFrom[C, B, That]): That =
    bf.fromSpecificIterable(coll)(new View.IntersperseSurround(seq(coll), start, sep, end))

}
