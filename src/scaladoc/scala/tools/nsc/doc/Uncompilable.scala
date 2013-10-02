/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package doc
import scala.language.implicitConversions
import scala.language.postfixOps

/** Some glue between DocParser (which reads source files which can't be compiled)
 *  and the scaladoc model.
 */
trait Uncompilable {
  val global: Global
  val settings: Settings

  import global.{ reporter, inform, warning, newTypeName, newTermName, Symbol, DocComment, NoSymbol }
  import global.definitions.AnyRefClass
  import global.rootMirror.RootClass

  private implicit def translateName(name: Global#Name) =
    if (name.isTypeName) newTypeName("" + name) else newTermName("" + name)

  def docSymbol(p: DocParser.Parsed) = p.nameChain.foldLeft(RootClass: Symbol)(_.tpe member _)
  def docDefs(code: String)          = new DocParser(settings, reporter) docDefs code
  def docPairs(code: String)         = docDefs(code) map (p => (docSymbol(p), new DocComment(p.raw)))

  lazy val pairs = files flatMap { f =>
    val comments = docPairs(f.slurp())
    if (settings.verbose)
      inform("Found %d doc comments in parse-only file %s: %s".format(comments.size, f, comments.map(_._1).mkString(", ")))

    comments
  }
  def files     = settings.uncompilableFiles
  def symbols   = pairs map (_._1)
  def templates = symbols filter (x => x.isClass || x.isTrait || x == AnyRefClass/* which is now a type alias */) toSet
  def comments = {
    if (settings.debug || settings.verbose)
      inform("Found %d uncompilable files: %s".format(files.size, files mkString ", "))

    if (pairs.isEmpty)
      warning("no doc comments read from " + settings.docUncompilable.value)

    pairs
  }
  override def toString = pairs.size + " uncompilable symbols:\n" + (
    symbols filterNot (_ == NoSymbol) map (x => "  " + x.owner.fullName + " " + x.defString) mkString "\n"
  )
}
