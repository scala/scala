/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/** An annotation to be applied to a match expression.  If present,
 *  the compiler will verify that the match has been compiled to a
 *  [[http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-3.html#jvms-3.10 tableswitch or lookupswitch]]
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
 *  Note: for pattern matches with one or two cases, the compiler generates jump instructions.
 *  Annotating such a match with `@switch` does not issue any warning.
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
final class switch extends scala.annotation.StaticAnnotation
