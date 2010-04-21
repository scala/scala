/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd
package program

import nsc._
import io._
import ast.parser.Tokens._

/** Given paths on the command line, tokenizes any scala files found
 *  and prints one token per line.
 */
object Tokens {
  private val tokensUsage = "Usage: tokens [options] <path1 path2 ...>\n\nOptions:"
  private val tokensOptions = List(
    "verbose" -> "be more verbose",
    "stats"   -> "output some stats"
  )
  private val tokensInfo = Simple.scalaProgramInfo("tokens", tokensUsage)
  private lazy val TokensSpec = Simple(tokensInfo, tokensOptions, Nil, null)

  def main(args0: Array[String]): Unit = {
    if (args0.isEmpty)
      return println(TokensSpec.helpMsg)

    val runner = TokensSpec instance args0
    import runner._

    val files = (residualArgs flatMap walk).distinct
    if (parsed isSet "verbose")
      println("Tokenizing: " + (files map (_.name) mkString " "))

    if (parsed isSet "stats")
      println("Stats not yet implemented.")

    files flatMap fromScalaSource foreach println
  }

  /** Given a path, returns all .scala files underneath it.
   */
  private def walk(arg: String): List[File] =
    Path(arg).walkFilter(x => x.isFile && x.hasExtension("scala")) map (_.toFile) toList

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
