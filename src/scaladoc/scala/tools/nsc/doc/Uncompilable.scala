/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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

  import global.{ reporter, inform, newTypeName, newTermName, runReporting, Symbol, DocComment, MethodSymbol, NoSymbol }
  import global.definitions.{ AnyRefClass, AnyRefTpe }
  import global.rootMirror.RootClass

  private implicit def translateName(name: Global#Name): global.Name =
    if (name.isTypeName) newTypeName("" + name) else newTermName("" + name)

  val WaitNames = List("scala", "AnyRef", "wait")

  def docSymbol(p: DocParser.Parsed) =
    p.nameChain match {
      // for scala.AnyRef.wait, pick Object#wait with the same arity
      case xs if xs.map(_.toString) == WaitNames =>
        val arity = p.docDef.definition match {
          case t: DocParser#DefDef => t.vparamss.flatten.size
          case _ => 0
        }
        val wait = AnyRefTpe.members find {
          case m: MethodSymbol =>
            m.name == newTermName("wait") && m.paramLists.flatten.size == arity
          case _ => false
        }
        wait.getOrElse(sys.error(s"Object#wait with $arity parameters was not found"))
      case xs => xs.foldLeft(RootClass: Symbol)(_.tpe member _)
    }
  def docDefs(code: String)          = new DocParser(settings, reporter) docDefs code
  def docPairs(code: String)         = docDefs(code) map (p => (docSymbol(p), DocComment(p.raw)))

  lazy val pairs = files flatMap { f =>
    val code = f.slurp()
    val comments = docPairs(code)
    if (settings.verbose.value) {
      inform(s"Found ${comments.size} doc comments in parse-only file $f: " + comments.map(_._1).mkString(", "))
    }
    comments
  }
  def files     = settings.uncompilableFiles
  def symbols   = pairs map (_._1)
  def templates = symbols.filter(x => x.isClass || x.isTrait || x == AnyRefClass/* which is now a type alias */).toSet
  def comments = {
    if (settings.isDebug || settings.verbose.value)
      inform("Found %d uncompilable files: %s".format(files.size, files mkString ", "))

    if (pairs.isEmpty)
      runReporting.warning(NoPosition, "no doc comments read from " + settings.docUncompilable.value, WarningCategory.Scaladoc, site = "")

    pairs
  }
  override def toString = "" + pairs.size + " uncompilable symbols:\n" + (
    symbols filterNot (_ == NoSymbol) map (x => "  " + x.owner.fullName + " " + x.defString) mkString "\n"
  )
}
