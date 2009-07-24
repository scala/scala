// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scala.tools.scalap
package scalax
package rules

trait Input[+A] extends Iterable[A] {

  def next : Result[Input[A], A, Nothing]
  def index : Int

  def iterator = new Iterator[A] {
    private var input : Input[A] = Input.this
    private var result = input.next

    def hasNext = result != Failure
    def next = {
      val Success(input, value) = result
      this.input = input
      this.result = input.next
      value
    }
  }
}


class ArrayInput[A](val array : Array[A], val index : Int) extends Input[A] {
  def this(array : Array[A]) = this(array, 0)

  lazy val next : Result[ArrayInput[A], A, Nothing] = if (index >= array.length) Failure
      else Success(new ArrayInput[A](array, index + 1), array(index))

  override lazy val toString = this.iterator.mkString("\"", "", "\"")
}


class IterableInput[A](iterator : Iterator[A], val index : Int) extends Input[A] {
  def this(iterable : Iterable[A]) = this(iterable.iterator, 0)

  lazy val next : Result[IterableInput[A], A, Nothing] = if (!iterator.hasNext) Failure
      else Success(new IterableInput(iterator, index + 1), iterator.next)

  override lazy val toString = this.iterator.mkString("\"", "", "\"")
}


/** View one type of input as another based on a transformation rule */
class View[A, B](
    transform : Input[A] => Result[Input[A], B, Nothing],
    val input : Input[A],
    val index : Int)
    extends Input[B] {

  def next : Result[Input[B], B, Nothing] = transform(input) match {
    case Success(context, b) => Success(new View(transform, context, index + 1), b)
    case _ => Failure
  }
}
