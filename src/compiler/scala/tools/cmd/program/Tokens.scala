/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd
package program

import nsc._
import util.Chars.char2uescape
import io._
import ast.parser.Tokens._

/** Given paths on the command line, tokenizes any scala files found
 *  and prints one token per line.
 */
object Tokens {
  private val tokensUsage = "Usage: tokens [options] <path1 path2 ...>\n\nOptions:"
  private val tokensUnary = List(
    "verbose" -> "be more verbose",
    "freq"    -> "combine token lists and sort by frequency",
    "stats"   -> "output some stats"
  )
  private val tokensBinary = List(
    "sliding" -> "print tokens in groups of given size"
  )
  private val tokensInfo = Simple.scalaProgramInfo("tokens", tokensUsage)
  private lazy val TokensSpec = Simple(tokensInfo, tokensUnary, tokensBinary, null)

  def sanitize(x: Any): String = sanitize(x.toString)
  def sanitize(str: String): String = str flatMap (x => if (x.isControl) char2uescape(x) else x.toString)

  def main(args0: Array[String]): Unit = {
    if (args0.isEmpty)
      return println(TokensSpec.helpMsg)

    val runner = TokensSpec instance args0
    import runner._

    val files = (residualArgs flatMap walk).distinct
    if (parsed isSet "--verbose")
      println("Tokenizing: " + (files map (_.name) mkString " "))

    if (parsed isSet "--stats")
      println("Stats not yet implemented.")

    def raw = files flatMap fromScalaSource
    def tokens: List[Any] =
      if (parsed isSet "--sliding") raw sliding parsed("--sliding").toInt map (_ map sanitize mkString " ") toList
      else raw

    def output =
      if (parsed isSet "--freq")
        (tokens groupBy (x => x) mapValues (_.length)).toList sortBy (-_._2) map (x => x._2 + " " + x._1)
      else
        tokens

    output foreach println
  }

  def fromPaths(paths: String*): List[Any] =
    (paths.toList flatMap walk).distinct flatMap fromScalaSource

  /** Given a path, returns all .scala files underneath it.
   */
  private def walk(arg: String): List[File] = {
    def traverse = Path(arg) ifDirectory (_.deepList()) getOrElse Iterator(File(arg))

    Path onlyFiles traverse filter (_ hasExtension "scala") toList
  }

  def fromScalaString(code: String): List[Any] = {
    val f = File.makeTemp("tokens")
    f writeAll code
    fromScalaSource(f)
  }

  /** Tokenizes a single scala file.
   */
  def fromScalaSource(file: Path): List[Any] = fromScalaSource(file.path)
  def fromScalaSource(file: String): List[Any] = {
    val global = new Global(new Settings())
    import global._
    import syntaxAnalyzer.{ UnitScanner, token2string }

    val in = new UnitScanner(new CompilationUnit(getSourceFile(file)))
    in.init()

    Iterator continually {
      val token = in.token match {
        case IDENTIFIER | BACKQUOTED_IDENT  => in.name
        case CHARLIT | INTLIT | LONGLIT     => in.intVal
        case DOUBLELIT | FLOATLIT           => in.floatVal
        case STRINGLIT                      => "\"" + in.strVal + "\""
        case SEMI | NEWLINE                 => ";"
        case NEWLINES                       => ";;"
        case COMMA                          => ","
        case EOF                            => null
        case x                              => token2string(x)
      }
      in.nextToken()
      token
    } takeWhile (_ != null) toList
  }
}
