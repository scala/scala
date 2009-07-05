/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

/**
 * Simple pattern types:
 *
 * 1 Variable               x
 * 3 Literal                56
 *
 * Types which must be decomposed into conditionals and simple types:
 *
 * 2 Typed                  x: Int
 * 4 Stable Identifier      Bob
 * 5 Constructor            Symbol("abc")
 * 6 Tuple                  (5, 5)
 * 7 Extractor              List(1, 2)
 * 8 Sequence               List(1, 2, _*)
 * 9 Infix                  5 :: xs
 * 10 Alternative           "foo" | "bar"
 * 11 XML                   --
 * 12 Regular Expression    --
 */

package scala.tools.nsc.matching

import util.Position
import ast.{ TreePrinters, Trees }
import symtab.SymbolTable
import java.io.{ StringWriter, PrintWriter }
import scala.util.NameTransformer.decode

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
trait TransMatcher extends ast.TreeDSL with CompactTreePrinter {
  self: transform.ExplicitOuter with PatternNodes with ParallelMatching =>

  import global.{ typer => _, _ }
  import analyzer.Typer;
  import definitions._
  import symtab.Flags
  import CODE._

  var cunit: CompilationUnit = _  // memory leak?
  var resultType: Type = _

  def newName(pos: Position, s: String) = cunit.fresh.newName(pos, s)

  final val settings_squeeze = settings.Xsqueeze.value == "on"

  /** Contains data structures which the match algorithm implementation
   *  requires but which aren't essential to the algorithm itself.
   */
  case class MatchMatrixContext(
    handleOuter: TreeFunction1,
    typer: Typer,
    owner: Symbol)
  {
    def newVar(
      pos: Position,
      tpe: Type,
      flags: List[Long] = Nil,
      name: Name = null): Symbol =
    {
      val n: Name = if (name == null) newName(pos, "temp") else name
      // careful: pos has special meaning
      owner.newVariable(pos, n) setInfo tpe setFlag (0L /: flags)(_|_)
    }

    def typedValDef(x: Symbol, rhs: Tree) = {
      val finalRhs = x.tpe match {
        case WildcardType   =>
          rhs setType null
          x setInfo (typer typed rhs).tpe
          rhs
        case _              =>
          typer.typed(rhs, x.tpe)
      }
      typer typed (VAL(x) === finalRhs)
    }

    def squeezedBlock(vds: List[Tree], exp: Tree): Tree =
      if (settings_squeeze) Block(Nil, squeezedBlock1(vds, exp))
      else                  Block(vds, exp)

    private def squeezedBlock1(vds: List[Tree], exp: Tree): Tree = {
      class RefTraverser(sym: Symbol) extends Traverser {
        var nref, nsafeRef = 0
        override def traverse(tree: Tree) = tree match {
          case t: Ident if t.symbol eq sym =>
            nref += 1
            if (sym.owner == currentOwner) // oldOwner should match currentOwner
              nsafeRef += 1

          case LabelDef(_, args, rhs) =>
            (args dropWhile(_.symbol ne sym)) match {
              case Nil  =>
              case _    => nref += 2  // cannot substitute this one
            }
            traverse(rhs)
          case t if nref > 1 =>       // abort, no story to tell
          case t =>
            super.traverse(t)
        }
      }

      class Subst(sym: Symbol, rhs: Tree) extends Transformer {
        var stop = false
        override def transform(tree: Tree) = tree match {
          case t: Ident if t.symbol == sym =>
            stop = true
            rhs
          case _ => if (stop) tree else super.transform(tree)
        }
      }

      lazy val squeezedTail = squeezedBlock(vds.tail, exp)
      def default = squeezedTail match {
        case Block(vds2, exp2) => Block(vds.head :: vds2, exp2)
        case exp2              => Block(vds.head :: Nil,  exp2)
      }

      if (vds.isEmpty) exp
      else vds.head match {
        case vd: ValDef =>
          val sym = vd.symbol
          val rt = new RefTraverser(sym)
          rt.atOwner (owner) (rt traverse squeezedTail)

          rt.nref match {
            case 0                      => squeezedTail
            case 1 if rt.nsafeRef == 1  => new Subst(sym, vd.rhs) transform squeezedTail
            case _                      => default
          }
        case _          =>
          default
      }
    }
  }

  /** handles all translation of pattern matching
   */
  def handlePattern(
    selector: Tree,
    cases: List[CaseDef],
    doCheckExhaustive: Boolean,
    owner: Symbol,
    handleOuter: TreeFunction1,
    localTyper: Typer): Tree =
  {
    val flags     = if (doCheckExhaustive) Nil else List(Flags.TRANS_FLAG)
    val context   = MatchMatrixContext(handleOuter, localTyper, owner)

    import context._

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

    val matrix  = new MatchMatrix(context, theFailTree)
    val rep     = matrix.execute(tmps, cases)
    val mch     = typer typed rep.toTree
    var dfatree = typer typed Block(vds, mch)

    // TRACE("handlePattern(\n  tmps = %s\n  cases = %s\n  rep = %s\n  initRep = %s\n)",
    //   tmps, cases.mkString("(\n    ", "\n    ", "\n)"), rep, irep)
    // TRACE("dfatree(1) = " + toCompactString(dfatree))

    // cannot use squeezedBlock because of side-effects, see t275
    for ((cs, bx) <- cases.zipWithIndex)
      if (!matrix.isReached(bx)) cunit.error(cs.body.pos, "unreachable code")

    dfatree = matrix cleanup dfatree
    resetTraverser traverse dfatree
    // TRACE("dfatree(2) = " + toCompactString(dfatree))
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
