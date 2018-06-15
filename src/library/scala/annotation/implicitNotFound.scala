/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * To customize the error message that's emitted when an implicit of type
 * `C[T1,..., TN]` cannot be found, annotate the class `C` with `@implicitNotFound`.
 * Assuming `C` has type parameters `X1, ..., XN`, the error message will be the
 * result of replacing all occurrences of `${Xi}` in the string `msg` with the
 * string representation of the corresponding type argument `Ti`.
 *
 * The annotation can also be attached to implicit parameters. In this case, `${Xi}`
 * can refer to type parameters in the current scope. The `@implicitNotFound` message
 * on the parameter takes precedence over the one on the parameter's type.
 *
 * {{{
 *   import scala.annotation.implicitNotFound
 *
 *   @implicitNotFound("Could not find an implicit C[${T}, ${U}]")
 *   class C[T, U]
 *
 *   class K[A] {
 *     def m[B](implicit c: C[List[A], B]) = 0
 *     def n[B](implicit @implicitNotFound("Specific message for C of list of ${A} and ${B}") c: C[List[A], B]) = 1
 *   }
 *
 *   object Test {
 *     val k = new K[Int]
 *     k.m[String]
 *     k.n[String]
 *   }
 * }}}
 *
 * The compiler issues the following error messages:
 *
 * <pre>
 * Test.scala:13: error: Could not find an implicit C[List[Int], String]
 *   k.m[String]
 *      ^
 * Test.scala:14: error: Specific message for C of list of Int and String
 *   k.n[String]
 *      ^
 * </pre>
 *
 *
 * @author Adriaan Moors
 * @since 2.8.1
 */
final class implicitNotFound(msg: String) extends scala.annotation.StaticAnnotation {}
