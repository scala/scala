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

trait Monad[+A] extends Functor[A] {
  type M[+A] <: Monad[A]
  def flatMap[B](f : A => M[B]) : M[B]
}

trait Monads extends UnitFunctors {
  type M[+A] <: Monad[A]

  trait Monad[+A] extends Functor[A] with rules.Monad[A] { this : M[A] =>
    def map[B](f : A => B) = flatMap { a => unit(f(a)) }
  }

  trait ZeroMonad extends Monad[Nothing] with ZeroFunctor { this : M[Nothing] =>
    def flatMap[B](f : Nothing => M[B]) : M[B] = this
  }
}


trait StateReader extends Monads {
  type S

  def get : M[S]
  def read[A](f : S => A) : M[A]
  def set(s : => S) : M[S]
  def update(f : S => S) : M[S]
}




