/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.combinator.syntactical

/** This is the core component for token-based parsers.
 *
 * <p>@requires lexical a component providing the tokens consumed by the parsers in this component</p>
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait TokenParsers extends Parsers {
  /** Tokens is the abstract type of the `Token's consumed by the parsers in this component*/
  type Tokens <: scala.util.parsing.syntax.Tokens

  /** lexical is the component responsible for consuming some basic kind of input (usually
   * character-based) and turning it into the tokens understood by these parsers.
   */
  val lexical: Tokens

  /** The input-type for these parsers*/
  type Elem = lexical.Token

  /** A parser generator delimiting whole phrases (i.e. programs).
   *<p>
   *  phrase(p) succeeds if `p' succeeds and no input is left over after `p'.</p>
   *
   * @param p the parser that must consume all input for the resulting parser to succeed
   * @return a parser that has the same result as `p', but that only succeeds if `p' consumed all the input
   */
  def phrase[t](p: Parser[t]) = new Parser[t] {
    def apply(in: Input) = p(in) match {
      case s @ Success(out, in1) if in1.atEnd => s
      case s @ Success(out, in1) => Failure("end of input expected", in1)
      case f @ Failure(_, in1) => in1.first match {
        case lexical.ErrorToken(msg)  => Failure(msg, in1)
        case lexical.EOF  => Failure("unexpected end of input", in1)
        case t  => Failure("unexpected token "+t, in1)
      }
      case f => f
    }
  }
}


