/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */

package scala.tools.nsc
package matching

import util.Position
import ast.{ TreePrinters, Trees }
import symtab.SymbolTable
import transform.ExplicitOuter
import java.io.{ StringWriter, PrintWriter }
import scala.util.NameTransformer.decode

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
trait TransMatcher extends ast.TreeDSL {
  self: ExplicitOuter with ParallelMatching  =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import definitions._
  import CODE._
  import Debug.tracing

  // cunit is set to the current unit in ExplicitOuter's transformUnit,
  // and nulled out afterward to avoid leaking.
  var cunit: CompilationUnit = _

  def newName(pos: Position, s: String) = cunit.fresh.newName(pos, s)

  final val settings_squeeze = settings.Xsqueeze.value == "on"

  /** Handles all translation of pattern matching.
   */
  def handlePattern(
    selector: Tree,         // tree being matched upon (called scrutinee after this)
    cases: List[CaseDef],   // list of cases in the match
    isChecked: Boolean,     // whether exhaustiveness checking is enabled (disabled with @unchecked)
    context: MatrixContext): Tree =
  {
    import context._

    def matchError(obj: Tree) = atPos(selector.pos)(THROW(MatchErrorClass, obj))
    def caseIsOk(c: CaseDef)  = cond(c.pat) { case _: Apply | Ident(nme.WILDCARD) => true }
    def rootTypes             = selector.tpe.typeArgs

    // this appears to be an attempt at optimizing when all case defs are constructor
    // patterns, but I don't think it's correct.
    def doApply(fn: Tree): Boolean =
      (fn.symbol eq (selector.tpe.decls lookup nme.CONSTRUCTOR)) &&
      (cases forall caseIsOk)

    // For x match { ... we start with a single root
    def singleMatch(): MatrixInit = {
      val v = copyVar(selector, isChecked)
      tracing("root(s)", context.MatrixInit(List(v), cases, matchError(v.ident)))
    }

    // For (x, y, z) match { ... we start with multiple roots, called tpXX.
    def tupleMatch(app: Apply): MatrixInit = {
      val Apply(fn, args) = app
      val vs = args zip rootTypes map { case (arg, tpe) => copyVar(arg, isChecked, tpe, "tp") }
      def merror = matchError(treeCopy.Apply(app, fn, vs map (_.ident)))

      tracing("root(s)", context.MatrixInit(vs, cases, merror))
    }

    // sets up top level variables and algorithm input
    val matrixInit = selector match {
      case app @ Apply(fn, _) if isTupleType(selector.tpe) && doApply(fn) => tupleMatch(app)
      case _                                                              => singleMatch()
    }

    val matrix  = new MatchMatrix(context) { lazy val data = matrixInit }
    val rep     = matrix.expansion                            // expands casedefs and assigns name
    val mch     = typer typed rep.toTree                      // executes algorithm, converts tree to DFA
    val dfatree = typer typed Block(matrixInit.valDefs, mch)  // packages into a code block

    // redundancy check
    matrix.targets filter (_.isNotReached) foreach (cs => cunit.error(cs.body.pos, "unreachable code"))
    // optimize performs squeezing and resets any remaining TRANS_FLAGs
    matrix optimize dfatree
  }

  object compactTreePrinters extends CompactTreePrinter

  private def toCompactString(t: Tree): String = {
    val buffer = new StringWriter()
    val printer = compactTreePrinters.create(new PrintWriter(buffer))
    printer.print(t)
    printer.flush()
    buffer.toString
  }
}

