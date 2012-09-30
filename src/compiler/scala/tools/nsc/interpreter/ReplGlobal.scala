/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import reporters._
import typechecker.Analyzer

/** A layer on top of Global so I can guarantee some extra
 *  functionality for the repl.  It doesn't do much yet.
 */
trait ReplGlobal extends Global {
  // This exists mostly because using the reporter too early leads to deadlock.
  private def echo(msg: String) { Console println msg }

  override def abort(msg: String): Nothing = {
    echo("ReplGlobal.abort: " + msg)
    super.abort(msg)
  }

  override lazy val analyzer = new {
    val global: ReplGlobal.this.type = ReplGlobal.this
  } with Analyzer {
    override def newTyper(context: Context): Typer = new Typer(context) {
      override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
        val res = super.typed(tree, mode, pt)
        tree match {
          case Ident(name) if !tree.symbol.hasPackageFlag && !name.toString.startsWith("$") =>
            repldbg("typed %s: %s".format(name, res.tpe))
          case _ =>
        }
        res
      }
    }
  }

  object replPhase extends SubComponent {
    val global: ReplGlobal.this.type = ReplGlobal.this
    val phaseName = "repl"
    val runsAfter = List[String]("typer")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      def apply(unit: CompilationUnit) {
        repldbg("Running replPhase on " + unit.body)
        // newNamer(rootContext(unit)).enterSym(unit.body)
      }
    }
  }

  override protected def computePhaseDescriptors: List[SubComponent] = {
    addToPhasesSet(replPhase, "repl")
    super.computePhaseDescriptors
  }
}
