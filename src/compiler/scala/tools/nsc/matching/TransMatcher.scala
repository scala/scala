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
trait TransMatcher extends ast.TreeDSL with CompactTreePrinter {
  self: ExplicitOuter with ParallelMatching with PatternOptimizer =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import definitions._
  import symtab.Flags
  import CODE._

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

    // TRANS_FLAG communicates that there should be no exhaustiveness checking
    val flags                 = List(Flags.TRANS_FLAG) filterNot (_ => isChecked)
    def matchError(obj: Tree) = atPos(selector.pos)(THROW(MatchErrorClass, obj))
    def caseIsOk(c: CaseDef)  = cond(c.pat) { case _: Apply | Ident(nme.WILDCARD) => true }

    // this appears to be an attempt at optimizing when all case defs are constructor
    // patterns, but I don't think it's correct.
    def doApply(fn: Tree): Boolean =
      (fn.symbol eq (selector.tpe.decls lookup nme.CONSTRUCTOR)) &&
      (cases forall caseIsOk)

    // For x match { ... we start with a single root
    def singleMatch(): (List[Tree], MatrixInit) = {
      val root: Symbol      = newVar(selector.pos, selector.tpe, flags)
      val varDef: Tree      = typedValDef(root, selector)

      (List(varDef), MatrixInit(List(root), cases, matchError(ID(root))))
    }

    // For (x, y, z) match { ... we start with multiple roots, called tpXX.
    def tupleMatch(app: Apply): (List[Tree], MatrixInit) = {
      val Apply(fn, args) = app
      val (roots, vars) = List.unzip(
        for ((arg, typeArg) <- args zip selector.tpe.typeArgs) yield {
          val v = newVar(arg.pos, typeArg, flags, newName(arg.pos, "tp"))
          (v, typedValDef(v, arg))
        }
      )
      (vars, MatrixInit(roots, cases, matchError(treeCopy.Apply(app, fn, roots map ID))))
    }

    // sets up top level variables and algorithm input
    val (vars, matrixInit) = selector match {
      case app @ Apply(fn, _) if isTupleType(selector.tpe) && doApply(fn) => tupleMatch(app)
      case _                                                              => singleMatch()
    }

    val matrix  = new MatchMatrix(context, matrixInit)
    val rep     = matrix.expansion                    // expands casedefs and assigns name
    val mch     = typer typed rep.toTree              // executes algorithm, converts tree to DFA
    val dfatree = typer typed Block(vars, mch)        // packages into a code block

    // redundancy check
    matrix.targets filter (_.isNotReached) foreach (cs => cunit.error(cs.body.pos, "unreachable code"))
    // optimize performs squeezing and resets any remaining TRANS_FLAGs
    matrix optimize dfatree
  }

  private def toCompactString(t: Tree): String = {
    val buffer = new StringWriter()
    val printer = compactTreePrinters.create(new PrintWriter(buffer))
    printer.print(t)
    printer.flush()
    buffer.toString
  }
}

/** A tree printer which is stingier about vertical whitespace and unnecessary
 *  punctuation than the standard one.
 */
trait CompactTreePrinter {
  val global: Global

  object compactTreePrinters extends {
    val trees: global.type = global
  } with TreePrinters {
    import trees._

    override def create(writer: PrintWriter): TreePrinter = new TreePrinter(writer) {
      // drill down through Blocks and pull out the real statements.
      def allStatements(t: Tree): List[Tree] = t match {
        case Block(stmts, expr) => (stmts flatMap allStatements) ::: List(expr)
        case _                  => List(t)
      }

      override def printRaw(tree: Tree): Unit = {
        // routing supercalls through this for debugging ease
        def s() = {
          // Console.println("toSuper: " + tree.getClass)
          super.printRaw(tree)
        }

        tree match {
          // labels used for jumps - does not map to valid scala code
          case LabelDef(name, params, rhs) =>
            print("labeldef %s(%s) = ".format(name, params mkString ","))
            printRaw(rhs)

          // target.method(arg) ==> target method arg
          case Apply(Select(target, method), List(arg)) =>
            (target, arg) match {
              case (_: Ident, _: Literal | _: Ident)  =>
                printRaw(target)
                print(" %s " format symName(tree, method))
                printRaw(arg)
              case _                        => s()
            }

          // case Select(Select(_, x), y) if x.toString == "this" =>
          //   print(symName(tree, y))
          // target.unary_! ==> !target
          case Select(qualifier, name) =>
            val n = symName(tree, name)
            if (n startsWith "unary_") {
              print(n drop 6)
              print(qualifier)
            }
            else s()

          // target.toString() ==> target.toString
          case Apply(fn, Nil)   => printRaw(fn)

          // if a Block only continues one actual statement, just print it.
          case Block(stats, expr) =>
            allStatements(tree) match {
              case List(x)            => printRow(List(x), "", ";", "")
              case _                  => s()
            }

          // If thenp or elsep has only one statement, it doesn't need more than one line.
          case If(cond, thenp, elsep) =>
            printRow(List(cond), "if (", "", ") ")

            def ifIndented(x: Tree) = {
              indent ; println ; printRaw(x) ; undent
            }

            indent ; println ;
            allStatements(thenp) match {
              case List(x: If)  => ifIndented(x)
              case List(x)      => printRaw(x)
              case _            => printRaw(thenp)
            }
            undent ; println ;
            val elseStmts = allStatements(elsep)
            if (!elseStmts.isEmpty) {
              print("else")
              indent ; println
              elseStmts match {
                case List(x)      => printRaw(x)
                case xs           => printRaw(elsep)
              }
              undent ; println
            }
          case _        => s()
        }
      }
    }
  }

  lazy val compactTreePrinter = compactTreePrinters.create()
}
