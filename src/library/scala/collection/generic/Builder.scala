/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $

package scala.collection.generic

/** The base trait of all builders.
 *  A builder lets one construct a collection incrementally, by adding elements
 *  to the builder with += and then converting to the required collection type with
 *  `result`.
 */
trait Builder[-Elem, +To, -From] extends Growable[Elem] {

  /** Adds a single element to the builder.
   *  @param elem The element to be added
   */
  def +=(elem: Elem)

  /** Clear the contents of this builder
   */
  def clear()

  /** Returns collection resulting from this builder. The buffer's contents are undefined afterwards.
   */
  def result(): To

  /** Give a hint how many elements are expected to be added
   *  when the next `result` is called.
   */
  def sizeHint(size: Int) {}

  /** Create a new builder which is the same as the current builder except that
   *  a given function is applied to the current builder's result.
   *  @param  f   the function to apply to the builder's result
   */
  def mapResult[NewTo](f: To => NewTo): Builder[Elem, NewTo, From] =
    new Builder[Elem, NewTo, From] with Proxy {
      val self = Builder.this
      def +=(x: Elem) = self += x
      def clear() = self.clear()
      override def ++=(xs: Iterator[Elem]) = self ++= xs
      override def ++=(xs: Traversible[Elem]) = self ++= xs
      def result: NewTo = f(self.result)
    }
}

