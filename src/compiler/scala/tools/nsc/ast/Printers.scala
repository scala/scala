/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import java.io.{ OutputStream, PrintWriter }

trait Printers extends scala.reflect.internal.Printers { this: Global =>

  import treeInfo.{ IsTrue, IsFalse }

  class TreePrinter(out: PrintWriter) extends super.TreePrinter(out) {

    override def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printPosition(tree)
        printTree(
            if (tree.isDef && tree.symbol != NoSymbol && tree.symbol.isInitialized) {
              tree match {
                case ClassDef(_, _, _, impl @ Template(ps, noSelfType, body))
                if (tree.symbol.thisSym != tree.symbol) =>
                  ClassDef(tree.symbol, Template(ps, ValDef(tree.symbol.thisSym), body))
                case ClassDef(_, _, _, impl)           => ClassDef(tree.symbol, impl)
                case ModuleDef(_, _, impl)             => ModuleDef(tree.symbol, impl)
                case ValDef(_, _, _, rhs)              => ValDef(tree.symbol, rhs)
                case DefDef(_, _, _, vparamss, _, rhs) => DefDef(tree.symbol, vparamss, rhs)
                case TypeDef(_, _, _, rhs)             => TypeDef(tree.symbol, rhs)
                case _ => tree
              }
            } else tree)
      case unit: CompilationUnit =>
        print("// Scala source: " + unit.source + "\n")
        if (unit.body == null) print("<null>")
        else { print(unit.body); println() }
        println()
        out.flush()
      case arg =>
        super.print(arg)
    }
  }

  // overflow cases missing from TreePrinter in scala.reflect.api
  override def xprintTree(treePrinter: super.TreePrinter, tree: Tree) = tree match {
    case DocDef(comment, definition) =>
      treePrinter.print(comment.raw)
      treePrinter.println()
      treePrinter.print(definition)

    case TypeTreeWithDeferredRefCheck() =>
      treePrinter.print("<tree with deferred refcheck>")

    case SelectFromArray(qualifier, name, _) =>
      treePrinter.print(qualifier, ".<arr>", symName(tree, name))

    case _ =>
      super.xprintTree(treePrinter, tree)
  }

  /** A tree printer which is stingier about vertical whitespace and unnecessary
   *  punctuation than the standard one.
   */
  class CompactTreePrinter(out: PrintWriter) extends TreePrinter(out) {
    override def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start)
      printSeq(ts)(print(_))(print(sep))
      print(end)
    }

    // drill down through Blocks and pull out the real statements.
    def allStatements(t: Tree): List[Tree] = t match {
      case Block(stmts, expr) => (stmts flatMap allStatements) ::: List(expr)
      case _                  => List(t)
    }

    def printLogicalOr(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
      printLogicalOp(t1, t2, "||")

    def printLogicalAnd(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
      printLogicalOp(t1, t2, "&&")

    def printLogicalOp(t1: (Tree, Boolean), t2: (Tree, Boolean), op: String) = {
      def maybenot(tvalue: Boolean) = if (tvalue) "" else "!"

      print("%s(" format maybenot(t1._2))
      printTree(t1._1)
      print(") %s %s(".format(op, maybenot(t2._2)))
      printTree(t2._1)
      print(")")
    }

    override def printTree(tree: Tree): Unit = {
      // routing supercalls through this for debugging ease
      def s() = super.printTree(tree)

      tree match {
        // labels used for jumps - does not map to valid scala code
        case LabelDef(name, params, rhs) =>
          print("labeldef %s(%s) = ".format(name, params mkString ","))
          printTree(rhs)

        case Ident(name) =>
          print(decodedSymName(tree, name))

        // target.method(arg) ==> target method arg
        case Apply(Select(target, method), List(arg)) =>
          if (method.decode.toString == "||")
            printLogicalOr(target -> true, arg -> true)
          else if (method.decode.toString == "&&")
            printLogicalAnd(target -> true, arg -> true)
          else (target, arg) match {
            case (_: Ident, _: Literal | _: Ident)  =>
              printTree(target)
              print(" ")
              printTree(Ident(method))
              print(" ")
              printTree(arg)
            case _                        => s()
          }

        // target.unary_! ==> !target
        case Select(qualifier, name) if (name.decode startsWith "unary_") =>
          print(name.decode drop 6)
          printTree(qualifier)

        case Select(qualifier, name) =>
          printTree(qualifier)
          print(".")
          print(quotedName(name, decode = true))

        // target.toString() ==> target.toString
        case Apply(fn, Nil)   => printTree(fn)

        // if a Block only continues one actual statement, just print it.
        case Block(stats, expr) =>
          allStatements(tree) match {
            case List(x)            => printTree(x)
            case xs                 => s()
          }

        // We get a lot of this stuff
        case If( IsTrue(), x, _)        => printTree(x)
        case If(IsFalse(), _, x)        => printTree(x)

        case If(cond,  IsTrue(), elsep)   =>  printLogicalOr(cond -> true, elsep -> true)
        case If(cond, IsFalse(), elsep)   => printLogicalAnd(cond -> false, elsep -> true)
        case If(cond,  thenp, IsTrue())   =>  printLogicalOr(cond -> false, thenp -> true)
        case If(cond,  thenp, IsFalse())  => printLogicalAnd(cond -> true, thenp -> true)

        // If thenp or elsep has only one statement, it doesn't need more than one line.
        case If(cond, thenp, elsep) =>
          def ifIndented(x: Tree) = {
            indent() ; println() ; printTree(x) ; undent()
          }

          val List(thenStmts, elseStmts) = List(thenp, elsep) map allStatements
          print("if ("); print(cond); print(") ")

          thenStmts match {
            case List(x: If)  => ifIndented(x)
            case List(x)      => printTree(x)
            case _            => printTree(thenp)
          }

          if (elseStmts.nonEmpty) {
            print(" else")
            indent() ; println()
            elseStmts match {
              case List(x)  => printTree(x)
              case _        => printTree(elsep)
            }
            undent() ; println()
          }
        case _        => s()
      }
    }
  }

  def asString(t: Tree): String = render(t, newStandardTreePrinter, settings.printtypes, settings.uniqid, settings.Yshowsymowners, settings.Yshowsymkinds)
  def asCompactString(t: Tree): String = render(t, newCompactTreePrinter, settings.printtypes, settings.uniqid, settings.Yshowsymowners, settings.Yshowsymkinds)
  def asCompactDebugString(t: Tree): String = render(t, newCompactTreePrinter, true, true, true, true)

  def newStandardTreePrinter(writer: PrintWriter): TreePrinter = new TreePrinter(writer)
  def newCompactTreePrinter(writer: PrintWriter): CompactTreePrinter = new CompactTreePrinter(writer)

  override def newTreePrinter(writer: PrintWriter): TreePrinter =
    if (settings.Ycompacttrees) newCompactTreePrinter(writer)
    else newStandardTreePrinter(writer)
  override def newTreePrinter(stream: OutputStream): TreePrinter = newTreePrinter(new PrintWriter(stream))
  override def newTreePrinter(): TreePrinter = newTreePrinter(new PrintWriter(ConsoleWriter))
}
