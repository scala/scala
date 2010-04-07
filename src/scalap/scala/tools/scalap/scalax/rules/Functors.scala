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

trait Functor[+A] {
  type M[+A] <: Functor[A]
  def map[B](f : A => B) : M[B]
}

trait Filter[+A] {
  type M[+A] <: Filter[A]
  def filter(f : A => Boolean) : M[A]
}

trait Plus[+A] {
  type M[+A] <: Plus[A]
  def plus[B >: A](other : => M[B]) : M[B]
}

trait OrElse[+A] {
  type M[+A] <: OrElse[A]
  def orElse[B >: A](other : => M[B]) : M[B]
}

trait Units {
  type M[+A]
  def unit : M[Unit]
  def unit[A](a : => A) : M[A]
}

trait Zero {
  type M[+A]
  def zero : M[Nothing]
}

trait Functors {
  type M[+A] <: Functor[A]

  trait Functor[+A] extends rules.Functor[A] { this : M[A] =>
    type M[+A] = Functors.this.M[A]
  }

  trait ZeroFunctor extends Functor[Nothing] { this : M[Nothing] =>
    override def map[B](f : Nothing => B) : M[B] = this
    def filter(f : Nothing => Boolean) : M[Nothing] = this
    def plus[B](other : => M[B]) : M[B] = other
    def orElse[B](other : => M[B]) : M[B] = other
  }
}

/** One of the 'unit' definitions must be overridden in concrete subclasses */
trait UnitFunctors extends Units with Functors {
  def unit : M[Unit] = unit(())
  def unit[A](a : => A) : M[A] = unit map { Unit => a }
}


trait Monoidals extends UnitFunctors {
  type M[+A] <: Monoidal[A]

  implicit def app[A, B](fab : M[A => B]) = (fa : M[A]) => fa applyTo fab
  implicit def appUnit[A, B](a2b : A => B) = app(unit(a2b))

  /** One of 'and' and 'applyTo' definitions must be overridden in concrete subclasses */
  trait Monoidal[+A] extends Functor[A] { self : M[A] =>
    def and[B](fb : => M[B]) : M[(A, B)] = ((a : A) => (b : B) => (a, b))(this)(fb)
    def applyTo[B](fab : M[A => B]) : M[B] = fab and this map { case (f, a) => f(a) }
  }
}
