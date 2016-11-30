/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/**
 * `ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
 * single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
 * Unit and, if the -Yliteral-types compiler option has been specified, singleton types
 * corresponding to value literals.
 *
 * Instances of `ValueOf[T]` are provided implicitly for all eligible types. Typically
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
@scala.annotation.implicitNotFound(msg = "No singleton value available for ${T}.")
final class ValueOf[T](val value: T) extends AnyVal
