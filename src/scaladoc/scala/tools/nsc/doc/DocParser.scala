/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package doc

import reporters._
import scala.reflect.internal.util._
import DocParser.Parsed

/** A very minimal global customized for extracting `DocDefs`.  It stops
 *  right after parsing so it can read `DocDefs` from source code which would
 *  otherwise cause the compiler to go haywire.
 */
class DocParser(settings: nsc.Settings, reporter: Reporter) extends Global(settings, reporter) with ScaladocGlobalTrait {
  def this(settings: Settings) = this(settings, new ConsoleReporter(settings))
  def this() = this(new Settings(Console println _))

  // the usual global initialization
  locally { new Run() }

  override def forScaladoc = true
  override protected def computeInternalPhases() {
    phasesSet += syntaxAnalyzer
  }

  /** Returns a list of `DocParser.Parseds`, which hold the DocDefs found
   *  in the given code along with the surrounding trees.
   */
  def docDefs(code: String) = {
    def loop(enclosing: List[Tree], tree: Tree): List[Parsed] = tree match {
      case x: PackageDef => x.stats flatMap (t => loop(enclosing :+ x, t))
      case x: DocDef     => new Parsed(enclosing, x) :: loop(enclosing :+ x.definition, x.definition)
      case x             => x.children flatMap (t => loop(enclosing, t))
    }
    loop(Nil, docUnit(code))
  }

  /** A compilation unit containing parsed source.
   */
  def docUnit(code: String) = {
    val unit    = new CompilationUnit(new BatchSourceFile("<console>", code))
    val scanner = newUnitParser(unit)

    scanner.compilationUnit()
  }
}

/** Since the DocParser's whole reason for existing involves trashing a
 *  global, it is designed to bottle up general `Global#Tree` types rather
 *  than path dependent ones.  The recipient will have to deal.
 */
object DocParser {
  type Tree    = Global#Tree
  type DefTree = Global#DefTree
  type DocDef  = Global#DocDef
  type Name    = Global#Name

  class Parsed(val enclosing: List[Tree], val docDef: DocDef) {
    def nameChain: List[Name] = (enclosing :+ docDef.definition) collect { case x: DefTree => x.name }
    def raw: String           = docDef.comment.raw

    override def toString = (
      nameChain.init.map(x => if (x.isTypeName) x + "#" else x + ".").mkString + nameChain.last
    )
  }
}
