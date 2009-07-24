/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.parsing.combinator.testing
import scala.util.parsing.combinator._

import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.syntactical.TokenParsers

/** <p>
 *    Facilitates testing a given parser on various input strings.
 *  </p>
 *  <p>
 *    Example use:
 *  </p><pre>
 *    <b>val</b> syntactic = <b>new</b> MyParsers</pre>
 *  <p>
 *    and
 *  </p><pre>
 *    <b>val</b> parser = syntactic.term</pre>
 *  <p>
 *    (if MyParsers extends TokenParsers with a parser called `term')
 *  </p>
 *
 * @author Martin Odersky, Adriaan Moors
 */
abstract class Tester {

  val syntactic: TokenParsers { val lexical: Lexical }
  val parser: syntactic.Parser[Any]


  /** Scans a String (using a `syntactic.lexical.Scanner'), parses it
   *  using <code>phrase(parser)</code>, and  prints the input and the
   *  parsed result to the console.
   */
  def test(in: String) {
    Console.println("\nin : "+in)
    Console.println(syntactic.phrase[Any](parser)(new syntactic.lexical.Scanner(in)))
  }
}
