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

/** Represents the combined value of two rules applied in sequence.
 *
 * @see the Scala parser combinator
 */
case class ~[+A, +B](_1: A, _2: B) {
  override def toString = "(" + _1 + " ~ " + _2 + ")"
}


sealed abstract class Result[+Out, +A, +X] {
  def out: Out
  def value: A
  def error: X

  implicit def toOption: Option[A]

  def map[B](f: A => B): Result[Out, B, X]
  def mapOut[Out2](f: Out => Out2): Result[Out2, A, X]
  def map[Out2, B](f: (Out, A) => (Out2, B)): Result[Out2, B, X]
  def flatMap[Out2, B](f: (Out, A) => Result[Out2, B, Nothing]): Result[Out2, B, X]
  def orElse[Out2 >: Out, B >: A](other: => Result[Out2, B, Nothing]): Result[Out2, B, X]
}

case class Success[+Out, +A](out: Out, value: A) extends Result[Out, A, Nothing] {
  def error = throw new ScalaSigParserError("No error")

  def toOption = Some(value)

  def map[B](f: A => B): Result[Out, B, Nothing] = Success(out, f(value))
  def mapOut[Out2](f: Out => Out2): Result[Out2, A, Nothing] = Success(f(out), value)
  def map[Out2, B](f: (Out, A) => (Out2, B)): Success[Out2, B] = f(out, value) match { case (out2, b) => Success(out2, b) }
  def flatMap[Out2, B](f: (Out, A) => Result[Out2, B, Nothing]): Result[Out2, B, Nothing]= f(out, value)
  def orElse[Out2 >: Out, B >: A](other: => Result[Out2, B, Nothing]): Result[Out2, B, Nothing] = this
}

sealed abstract class NoSuccess[+X] extends Result[Nothing, Nothing, X] {
  def out = throw new ScalaSigParserError("No output")
  def value = throw new ScalaSigParserError("No value")

  def toOption = None

  def map[B](f: Nothing => B) = this
  def mapOut[Out2](f: Nothing => Out2) = this
  def map[Out2, B](f: (Nothing, Nothing) => (Out2, B)) = this
  def flatMap[Out2, B](f: (Nothing, Nothing) => Result[Out2, B, Nothing]) = this
  def orElse[Out2, B](other: => Result[Out2, B, Nothing]) = other
}

case object Failure extends NoSuccess[Nothing] {
  def error = throw new ScalaSigParserError("No error")
}

case class ScalaSigParserError(msg: String) extends RuntimeException(msg)

case class Error[+X](error: X) extends NoSuccess[X]
