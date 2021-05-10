/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package doc

import scala.language.implicitConversions

import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.Reporting.WarningCategory

/** Some glue between DocParser (which reads source files which can't be compiled)
 *  and the scaladoc model.
 */
trait Uncompilable {
  val global: Global
  val settings: Settings

  import global.{ reporter, inform, newTypeName, newTermName, runReporting, Symbol, DocComment, NoSymbol }
  import global.definitions.AnyRefClass
  import global.rootMirror.RootClass

  private implicit def translateName(name: Global#Name) =
    if (name.isTypeName) newTypeName("" + name) else newTermName("" + name)

  def docSymbol(p: DocParser.Parsed) = p.nameChain.foldLeft(RootClass: Symbol)(_.tpe member _)
  def docDefs(code: String)          = new DocParser(settings, reporter) docDefs code
  def docPairs(code: String)         = docDefs(code) map (p => (docSymbol(p), DocComment(p.raw)))

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
    if (settings.isDebug || settings.verbose)
      inform("Found %d uncompilable files: %s".format(files.size, files mkString ", "))

    if (pairs.isEmpty)
      runReporting.warning(NoPosition, "no doc comments read from " + settings.docUncompilable.value, WarningCategory.Scaladoc, site = "")

    pairs
  }
  override def toString = pairs.size + " uncompilable symbols:\n" + (
    symbols filterNot (_ == NoSymbol) map (x => "  " + x.owner.fullName + " " + x.defString) mkString "\n"
  )
}
