/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: TokenParsers.scala 12242 2007-07-09 09:43:09Z michelou $


package scala.util.parsing.combinator1.syntactical

/** <p>
 *    This is the core component for token-based parsers.
 *  </p>
 *  <p>
 *    @requires lexical a component providing the tokens consumed by the
 *    parsers in this component.
 *  </p>
 *
 *  @author Martin Odersky, Adriaan Moors
 */
trait TokenParsers extends Parsers {
  /** Tokens is the abstract type of the `Token's consumed by the parsers in this component*/
  type Tokens <: scala.util.parsing.syntax.Tokens

  /** lexical is the component responsible for consuming some basic kind of
   *  input (usually character-based) and turning it into the tokens
   *  understood by these parsers.
   */
  val lexical: Tokens

  /** The input-type for these parsers*/
  type Elem = lexical.Token

  /** <p>
   *    A parser generator delimiting whole phrases (i.e. programs).
   *  </p>
   *  <p>
   *    <code>phrase(p)</code> succeeds if <code>p</code> succeeds and
   *    no input is left over after <code>p</code>.
   *  </p>
   *
   *  @param p the parser that must consume all input for the resulting parser
   *           to succeed.
   *  @return  a parser that has the same result as `p', but that only succeeds
   *           if <code>p</code> consumed all the input.
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


