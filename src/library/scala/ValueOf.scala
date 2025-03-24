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

/**
 * `ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
 * single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
 * Unit and singleton types corresponding to value literals.
 *
 * The value itself can conveniently be retrieved with [[Predef#valueOf]], which requires
 * a `ValueOf` to be available in implicit scope.
 *
 * The compiler provides instances of `ValueOf[T]` for all eligible types. Typically
 * an instance would be required where a runtime value corresponding to a type level
 * computation is needed.

 * For example, we might define a type `Residue[M <: Int]` corresponding to the group of
 * integers modulo `M`. We could then mandate that residues can be summed only when they
 * are parameterized by the same modulus,
 *
 * {{{
 * case class Residue[M <: Int](n: Int) extends AnyVal {
 *   def +(rhs: Residue[M])(implicit m: ValueOf[M]): Residue[M] =
 *     Residue((this.n + rhs.n) % valueOf[M])
 * }
 *
 * val fiveModTen = Residue[10](5)
 * val nineModTen = Residue[10](9)
 *
 * fiveModTen + nineModTen    // OK == Residue[10](4)
 *
 * val fourModEleven = Residue[11](4)
 *
 * fiveModTen + fourModEleven // compiler error: type mismatch;
 *                            //   found   : Residue[11]
 *                            //   required: Residue[10]
 * }}}
 *
 * Notice that here the modulus is encoded in the type of the values and so does not
 * incur any additional per-value storage cost. When a runtime value of the modulus
 * is required in the implementation of `+` it is provided at the call site via the
 * implicit argument `m` of type `ValueOf[M]`.
 */
@scala.annotation.implicitNotFound(msg = "No singleton value available for ${T}; eligible singleton types for `ValueOf` synthesis include literals and stable paths.")
final class ValueOf[T](val value: T) extends AnyVal
