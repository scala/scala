/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime

/** A common supertype for companion classes of primitive types.
 *
 *  A common trait for /companion/ objects of primitive types comes handy
 *  when parameterizing code on types. For instance, the specialized
 *  annotation is passed a sequence of types on which to specialize:
 *  {{{
 *     class Tuple1[@specialized(Unit, Int, Double) T]
 *  }}}
 *
 */
sealed trait AnyValCompanion

/** A object representing `object scala.Unit`. It should never be used
 *  directly.
 */
object Unit extends AnyValCompanion {
  override def toString = "object scala.Unit"
}

/** A object representing `object scala.Boolean`. It should never be used
 *  directly.
 */
object Boolean extends AnyValCompanion {
  override def toString = "object scala.Boolean"
}

/** A object representing `object scala.Byte`. It should never be used
 *  directly.
 */
object Byte extends AnyValCompanion {
  override def toString = "object scala.Byte"
}

/** A object representing `object scala.Short`. It should never be used
 *  directly.
 */
object Short extends AnyValCompanion {
  override def toString = "object scala.Short"
}

/** A object representing `object scala.Char`. It should never be used
 *  directly.
 */
object Char extends AnyValCompanion {
  override def toString = "object scala.Char"
}

/** A object representing `object scala.Int`. It should never be used
 *  directly.
 */
object Int extends AnyValCompanion {
  override def toString = "object scala.Int"
}

/** A object representing `object scala.Long`. It should never be used
 *  directly.
 */
object Long extends AnyValCompanion {
  override def toString = "object scala.Long"
}

/** A object representing `object scala.Float`. It should never be used
 *  directly.
 */
object Float extends AnyValCompanion {
  override def toString = "object scala.Float"
}

/** A object representing `object scala.Double`. It should never be used
 *  directly.
 */
object Double extends AnyValCompanion {
  override def toString = "object scala.Double"
}
