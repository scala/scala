/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

import util.Position
import ast.{ TreePrinters, Trees }
import symtab.SymbolTable
import java.io.{ StringWriter, PrintWriter }

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
trait TransMatcher extends ast.TreeDSL with CompactTreePrinter {
  self: transform.ExplicitOuter with PatternNodes with ParallelMatching with CodeFactory =>

  import global.{ typer => _, _ }
  import analyzer.Typer;
  import definitions._
  import symtab.Flags
  import CODE._

  var cunit: CompilationUnit = _  // memory leak?
  var resultType: Type = _

  def newName(pos: Position, s: String) = cunit.fresh.newName(pos, s)

  final val settings_squeeze = settings.Xsqueeze.value == "on"

  // check special case Seq(p1,...,pk,_*)
  protected def isRightIgnoring(p: ArrayValue): Boolean =
    !p.elems.isEmpty && cond(unbind(p.elems.last)) { case Star(q) => isDefaultPattern(q) }

  /** handles all translation of pattern matching
   */
  def handlePattern(
    selector: Tree,
    cases: List[CaseDef],
    doCheckExhaustive: Boolean,
    owner: Symbol,
    handleOuter: Tree => Tree)
    (implicit typer: Typer): Tree =
  {
    implicit val theOwner = owner
    implicit val rep = new RepFactory(handleOuter)
    val flags = if (doCheckExhaustive) Nil else List(Flags.TRANS_FLAG)

    def matchError(obj: Tree)   = atPos(selector.pos)(THROW(MatchErrorClass, obj))
    def caseIsOk(c: CaseDef)    = cond(c.pat) { case _: Apply | Ident(nme.WILDCARD) => true }

    def doApply(fn: Tree): Boolean =
      (fn.symbol eq (selector.tpe.decls lookup nme.CONSTRUCTOR)) &&
      (cases forall caseIsOk)

    def processTuple(app: Apply): (List[Symbol], List[Tree], Tree) = {
      val Apply(fn, args) = app
      val (tmps, vds) = List.unzip(
        for ((arg, typeArg) <- args zip selector.tpe.typeArgs) yield {
          val v = newVar(arg.pos, typeArg, flags, newName(arg.pos, "tp"))
          (v, typedValDef(v, arg))
        }
      )
      (tmps, vds, matchError(treeCopy.Apply(app, fn, tmps map ID)))
    }

    // sets temporaries, variable declarations, and the fail tree
    val (tmps, vds, theFailTree) = selector match {
      case app @ Apply(fn, _) if isTupleType(selector.tpe) && doApply(fn) =>
        processTuple(app)
      case _ =>
        val root: Symbol      = newVar(selector.pos, selector.tpe, flags)
        val vdef: Tree        = typer typed (VAL(root) === selector)
        val failTree: Tree    = matchError(ID(root))
        (List(root), List(vdef), failTree)
    }

    implicit val fail: Tree = theFailTree
    val irep = initRep(tmps, cases, rep)
    val mch = typer typed irep.toTree
    var dfatree = typer typed Block(vds, mch)

    TRACE("handlePattern(\n  tmps = %s\n  cases = %s\n  rep = %s\n  initRep = %s\n)",
      tmps, cases.mkString("(\n    ", "\n    ", "\n)"), rep, irep)
    TRACE("dfatree(1) = " + toCompactString(dfatree))

    // cannot use squeezedBlock because of side-effects, see t275
    for ((cs, bx) <- cases.zipWithIndex)
      if (!rep.isReached(bx)) cunit.error(cs.body.pos, "unreachable code")

    dfatree = rep cleanup dfatree
    resetTraverser traverse dfatree
    TRACE("dfatree(2) = " + toCompactString(dfatree))
    dfatree
  }

  private def toCompactString(t: Tree): String = {
    val buffer = new StringWriter()
    val printer = compactTreePrinters.create(new PrintWriter(buffer))
    printer.print(t)
    printer.flush()
    buffer.toString
  }

  private object resetTraverser extends Traverser {
    override def traverse(x: Tree): Unit = x match {
      case vd: ValDef =>
        if (vd.symbol hasFlag Flags.SYNTHETIC) {
          vd.symbol resetFlag (Flags.TRANS_FLAG | Flags.MUTABLE)
        }
      case _ =>
        super.traverse(x)
    }
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
                print(" %s " format method)
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

            allStatements(thenp) match {
              case List(x)  => printRow(List(x), "", ";", "")
              case _        => printRaw(thenp)
            }
            println
            allStatements(elsep) match {
              case Nil      =>
              case List(x)  => printRow(List(x), "else ", "", "")
              case xs       => print("else ") ; printRaw(elsep)
            }
          case _        => s()
        }
      }
      // override def symName(tree: Tree, name: Name): String =
      //   super.symName(tree, name).replaceAll("""^(.*)\.""", "")
    }
  }

  lazy val compactTreePrinter = compactTreePrinters.create()
}
