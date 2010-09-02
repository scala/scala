/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.parsing.json

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

/**
 *  A marker class for the JSON result types.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
sealed abstract class JSONType

/**
 *  Represents a JSON Object (map).
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
case class JSONObject (obj : Map[Any,Any]) extends JSONType {
  override def toString = "{" + obj.map({ case (k,v) => k + " : " + v }).mkString(", ") + "}"
}

/**
 *  Represents a JSON Array (list).
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
case class JSONArray (list : List[Any]) extends JSONType {
  override def toString = "[" + list.mkString(", ") + "]"
}

/**
 *  The main JSON Parser.
 *
 *  @author Derek Chen-Becker <"java"+@+"chen-becker"+"."+"org">
 */
class Parser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = Lexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  /** Type signature for functions that can parse numeric literals */
  type NumericParser = String => Any

  // Global default number parsing function
  protected var defaultNumberParser : NumericParser = {_.toDouble}

  // Per-thread default number parsing function
  protected val numberParser = new ThreadLocal[NumericParser]() {
    override def initialValue() = defaultNumberParser
  }

  // Define the grammar
  def root       = jsonObj | jsonArray
  def jsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[_] => JSONObject(Map(vals : _*)) }
  def jsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals : List[_] => JSONArray(vals) }
  def objEntry   = stringVal ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[Any] = (jsonObj | jsonArray | number | "true" ^^^ true | "false" ^^^ false | "null" ^^^ null | stringVal)
  def stringVal  = accept("string", { case lexical.StringLit(n) => n} )
  def number     = accept("number", { case lexical.NumericLit(n) => numberParser.get.apply(n)} )
}

