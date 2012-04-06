/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/** An annotation to be applied to a match expression.  If present,
 *  the compiler will verify that the match has been compiled to a
 *  [[http://docs.oracle.com/javase/specs/jvms/se5.0/html/Instructions2.doc14.html tableswitch]]
 *  or [[http://docs.oracle.com/javase/specs/jvms/se5.0/html/Instructions2.doc8.html#lookupswitch lookupswitch]]
 *  and issue an error if it instead compiles into a series of conditional expressions.
 *  Example usage:
{{{
  val Constant = 'Q'
  def tokenMe(ch: Char) = (ch: @switch) match {
    case ' ' | '\t' | '\n'  => 1
    case 'A' | 'Z' | '$'    => 2
    case '5' | Constant     => 3  // a non-literal may prevent switch generation: this would not compile
    case _                  => 4
  }
}}}
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
final class switch extends annotation.StaticAnnotation
