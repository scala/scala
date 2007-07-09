/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.json;

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

/**
 *  @author Derek Chen-Becker <java@chen-becker.org>
 */
class Parser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  // Define the grammar
  def root       = jsonObj | jsonArray
  def jsonObj    = "{" ~ repsep(objEntry,",") ~ "}"
  def jsonArray  = "[" ~ repsep(value, ",") ~ "]"
  def objEntry   = stringVal ~ ":" ~ value ^^ { case x ~ y => (x,y) }
  def value: Parser[Any] = (jsonObj | jsonArray | number | "true" ^^ true | "false" ^^ false | "null" ^^ null | stringVal)
  def stringVal  = accept("string", {case lexical.StringLit(n) => n})
  def number     = accept("number", {case lexical.NumericLit(n) => n.toDouble})
}

