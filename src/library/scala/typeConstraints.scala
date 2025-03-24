/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.annotation.implicitNotFound

/** An instance of `A <:< B` witnesses that `A` is a subtype of `B`.
  *  Requiring an implicit argument of the type `A <:< B` encodes
  *  the generalized constraint `A <: B`.
  *
  *  To constrain any abstract type `T` that's in scope in a method's
  *  argument list (not just the method's own type parameters) simply
  *  add an implicit argument of type `T <:< U`, where `U` is the required
  *  upper bound; or for lower-bounds, use: `L <:< T`, where `L` is the
  *  required lower bound.
  *
  *  In case of any confusion over which method goes in what direction, all the "Co" methods (including
  *  [[apply]]) go from left to right in the type ("with" the type), and all the "Contra" methods go
  *  from right to left ("against" the type). E.g., [[apply]] turns a `From` into a `To`, and
  *  [[substituteContra]] replaces the `To`s in a type with `From`s.
  *
  *  In part contributed by Jason Zaugg.
  *
  *  @tparam From a type which is proved a subtype of `To`
  *  @tparam To a type which is proved a supertype of `From`
  *
  *  @example [[scala.Option#flatten]]
  *           {{{
  *            sealed trait Option[+A] {
  *              // def flatten[B, A <: Option[B]]: Option[B] = ...
  *              // won't work, since the A in flatten shadows the class-scoped A.
  *              def flatten[B](implicit ev: A <:< Option[B]): Option[B]
  *                = if(isEmpty) None else ev(get)
  *              // Because (A <:< Option[B]) <: (A => Option[B]), ev can be called to turn the
  *              // A from get into an Option[B], and because ev is implicit, that call can be
  *              // left out and inserted automatically.
  *            }
  *           }}}
  *
  *  @see [[=:=]] for expressing equality constraints
  *
  *  @define isProof This method is impossible to implement without `throw`ing or otherwise "cheating" unless
  *                  `From <: To`, so it ensures that this really represents a subtyping relationship.
  *  @define contraCo contravariant in the first argument and covariant in the second
  *  @define contraCon a contravariant type constructor
  *  @define coCon a covariant type constructor
  *  @define sameDiff but with a (potentially) different type
  *  @define tp <:<
  */
// All of these methods are reimplemented unsafely in =:=.singleton to avoid any indirection.
// They are here simply for reference as the "correct", safe implementations.
@implicitNotFound(msg = "Cannot prove that ${From} <:< ${To}.")
sealed abstract class <:<[-From, +To] extends (From => To) with Serializable {
  /** Substitute `To` for `From` and `From` for `To` in the type `F[To, From]`, given that `F` is $contraCo.
    *  Essentially swaps `To` and `From` in `ftf`'s type.
    *
    *  Equivalent in power to each of [[substituteCo]] and [[substituteContra]].
    *
    *  $isProof
    *
    *  @return `ftf`, $sameDiff
    */
  def substituteBoth[F[-_, +_]](ftf: F[To, From]): F[From, To]
  // = substituteCo[({type G[+T] = F[From, T]})#G](substituteContra[({type G[-T] = F[T, From})#G](ftf))
  // = substituteContra[({type G[-T] = F[T, To]})#G](substituteCo[({type G[+T] = F[From, T]})#G](ftf))
  /** Substitute the `From` in the type `F[From]`, where `F` is $coCon, for `To`.
    *
    *  Equivalent in power to each of [[substituteBoth]] and [[substituteContra]].
    *
    *  $isProof
    *
    *  @return `ff`, $sameDiff
    */
  def substituteCo[F[+_]](ff: F[From]): F[To] = {
    type G[-_, +T] = F[T]
    substituteBoth[G](ff)
  }
  // = substituteContra[({type G[-T] = F[T] => F[To]})#G](identity)(ff)
  /** Substitute the `To` in the type `F[To]`, where `F` is $contraCon, for `From`.
    *
    *  Equivalent in power to each of [[substituteBoth]] and [[substituteCo]].
    *
    *  $isProof
    *
    *  @return `ft`, $sameDiff
    */
  def substituteContra[F[-_]](ft: F[To]): F[From] = {
    type G[-T, +_] = F[T]
    substituteBoth[G](ft)
  }
  // = substituteCo[({type G[+T] = F[T] => F[From]})#G](identity)(ft)

  /** Coerce a `From` into a `To`. This is guaranteed to be the identity function.
    *
    *  This method is often called implicitly as an implicit `A $tp B` doubles as an implicit view `A => B`.
    *
    *  @param f some value of type `From`
    *  @return `f`, $sameDiff
    */
  override def apply(f: From): To = {
    type Id[+X] = X
    substituteCo[Id](f)
  }

  override def compose[C](r: C => From): C => To = {
    type G[+T] = C => T
    substituteCo[G](r)
  }
  /** If `From <: To` and `C <: From`, then `C <: To` (subtyping is transitive) */
  def compose[C](r: C <:< From): C <:< To = {
    type G[+T] = C <:< T
    substituteCo[G](r)
  }
  override def andThen[C](r: To => C): From => C = {
    type G[-T] = T => C
    substituteContra[G](r)
  }
  /** If `From <: To` and `To <: C`, then `From <: C` (subtyping is transitive) */
  def andThen[C](r: To <:< C): From <:< C = {
    type G[-T] = T <:< C
    substituteContra[G](r)
  }

  /** Lift this evidence over $coCon `F`. */
  def liftCo[F[+_]]: F[From] <:< F[To] = {
    type G[+T] = F[From] <:< F[T]
    substituteCo[G](implicitly[G[From]])
  }
  /** Lift this evidence over $contraCon `F`. */
  def liftContra[F[-_]]: F[To] <:< F[From] = {
    type G[-T] = F[To] <:< F[T]
    substituteContra[G](implicitly[G[To]])
  }
}

object <:< {
  // the only instance for <:< and =:=, used to avoid overhead
  private val singleton: =:=[Any, Any] = new =:=[Any,Any] {
    override def substituteBoth[F[_, _]](ftf: F[Any, Any]) = ftf
    override def substituteCo    [F[_]](ff: F[Any]) = ff
    override def substituteContra[F[_]](ff: F[Any]) = ff
    override def apply(x: Any) = x
    override def flip: Any =:= Any = this
    override def compose[C](r: C =>  Any) = r
    override def compose[C](r: C <:< Any) = r
    override def compose[C](r: C =:= Any) = r
    override def andThen[C](r: Any =>  C) = r
    override def andThen[C](r: Any <:< C) = r
    override def andThen[C](r: Any =:= C) = r
    override def liftCo    [F[_]] = asInstanceOf[F[Any] =:= F[Any]]
    override def liftContra[F[_]] = asInstanceOf[F[Any] =:= F[Any]]
    override def toString = "generalized constraint"
  }

  /** `A =:= A` for all `A` (equality is reflexive). This also provides implicit views `A <:< B`
   *  when `A <: B`, because `(A =:= A) <: (A <:< A) <: (A <:< B)`.
   */
  implicit def refl[A]: A =:= A = singleton.asInstanceOf[A =:= A]
  // = new =:=[A, A] { override def substituteBoth[F[_, _]](faa: F[A, A]): F[A, A] = faa }

  /** If `A <: B` and `B <: A`, then `A = B` (subtyping is antisymmetric) */
  def antisymm[A, B](implicit l: A <:< B, r: B <:< A): A =:= B = singleton.asInstanceOf[A =:= B]
  // = ??? (I don't think this is possible to implement "safely")
}

/** An instance of `A =:= B` witnesses that the types `A` and `B` are equal. It also acts as a `A <:< B`,
  *  but not a `B <:< A` (directly) due to restrictions on subclassing.
  *
  *  In case of any confusion over which method goes in what direction, all the "Co" methods (including
  *  [[apply]]) go from left to right in the type ("with" the type), and all the "Contra" methods go
  *  from right to left ("against" the type). E.g., [[apply]] turns a `From` into a `To`, and
  *  [[substituteContra]] replaces the `To`s in a type with `From`s.
  *
  *  @tparam From a type which is proved equal to `To`
  *  @tparam To a type which is proved equal to `From`
  *
  *  @example An in-place variant of [[scala.collection.mutable.ArrayBuffer#transpose]] {{{
  *            implicit class BufOps[A](private val buf: ArrayBuffer[A]) extends AnyVal {
  *              def inPlaceTranspose[E]()(implicit ev: A =:= ArrayBuffer[E]) = ???
  *              // Because ArrayBuffer is invariant, we can't make do with just a A <:< ArrayBuffer[E]
  *              // Getting buffers *out* from buf would work, but adding them back *in* wouldn't.
  *            }
  *           }}}
  *  @see [[<:<]] for expressing subtyping constraints
  *
  *  @define isProof This method is impossible to implement without `throw`ing or otherwise "cheating" unless
  *                  `From = To`, so it ensures that this really represents a type equality.
  *  @define contraCo a type constructor of two arguments
  *  @define contraCon any type constructor
  *  @define coCon any type constructor
  *  @define tp =:=
  */
// Most of the notes on <:< above apply to =:= as well
@implicitNotFound(msg = "Cannot prove that ${From} =:= ${To}.")
sealed abstract class =:=[From, To] extends (From <:< To) with Serializable {
  override def substituteBoth[F[_, _]](ftf: F[To, From]): F[From, To]
  override def substituteCo[F[_]](ff: F[From]): F[To] = {
    type G[_, T] = F[T]
    substituteBoth[G](ff)
  }
  // = substituteContra[({type G[T] = F[T] => F[To]})#G](identity)(ff)
  override def substituteContra[F[_]](ft: F[To]): F[From] = {
    type G[T, _] = F[T]
    substituteBoth[G](ft)
  }
  // = substituteCo[({type G[T] = F[T] => F[From]})#G](identity)(ft)

  /** @inheritdoc */ override def apply(f: From) = super.apply(f)

  /** If `From = To` then `To = From` (equality is symmetric) */
  def flip: To =:= From = {
    type G[T, F] = F =:= T
    substituteBoth[G](this)
  }

  /** If `From = To` and `C = From`, then `C = To` (equality is transitive) */
  def compose[C](r: C =:= From): C =:= To = {
    type G[T] = C =:= T
    substituteCo[G](r)
  }
  /** If `From = To` and `To = C`, then `From = C` (equality is transitive) */
  def andThen[C](r: To =:= C): From =:= C = {
    type G[T] = T =:= C
    substituteContra[G](r)
  }

  override def liftCo[F[_]]: F[From] =:= F[To] = {
    type G[T] = F[T] =:= F[To]
    substituteContra[G](implicitly[G[To]])
  }
  /** Lift this evidence over the type constructor `F`, but flipped. */
  override def liftContra[F[_]]: F[To] =:= F[From] = liftCo[F].flip
}
