/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.compat.Platform.EOL
import symtab.Flags._
import scala.language.postfixOps
import scala.reflect.internal.util.ListOfNil

/** The object `nodePrinter` converts the internal tree
 *  representation to a string.
 *
 *  @author  Stephane Micheloud
 *  @author  Paul Phillips
 */
abstract class NodePrinters {
  val global: Global
  import global._

  object InfoLevel extends Enumeration {
    val Quiet, Normal, Verbose = Value
  }
  var infolevel = InfoLevel.Quiet

  def nodeToString: Tree => String = nodeToRegularString

  object nodeToRegularString extends DefaultPrintAST with (Tree => String) {
    def apply(tree: Tree) = stringify(tree)
  }

  trait DefaultPrintAST extends PrintAST {
    val printPos = settings.Xprintpos || settings.Yposdebug

    def showNameAndPos(tree: NameTree) = showPosition(tree) + showName(tree.name)
    def showDefTreeName(tree: DefTree) = showName(tree.name)
    def showPosition(tree: Tree)       = if (printPos) tree.pos.show else ""
    def showFlags(tree: MemberDef)     = flagsToString(tree.symbol.flags | tree.mods.flags)
    def showLiteral(lit: Literal)      = showPosition(lit) + lit.value.escapedStringValue
    def showTypeTree(tt: TypeTree)     = showPosition(tt) + "<tpt>" + emptyOrComment(showType(tt))
    def showName(name: Name)           = name match {
      case nme.EMPTY | tpnme.EMPTY => "<empty>"
      case name                    => "\"" + name + "\""
    }

    def showSymbol(tree: Tree): String = {
      val sym = tree.symbol
      if (sym == null || sym == NoSymbol) ""
      else sym.defString + sym.locationString
    }
    def showType(tree: Tree): String = {
      val tpe = tree.tpe
      if (tpe == null || tpe == NoType) ""
      else "tree.tpe=" + tpe
    }

    def showAttributes(tree: Tree): String = {
      if (infolevel == InfoLevel.Quiet) ""
      else {
        try   { List(showSymbol(tree), showType(tree)) filterNot (_ == "") mkString ", " trim }
        catch { case ex: Throwable => "sym= <error> " + ex.getMessage }
      }
    }
  }

  trait PrintAST {
    private val buf = new StringBuilder
    private var level = 0

    def showName(name: Name): String
    def showPosition(tree: Tree): String
    def showNameAndPos(tree: NameTree): String
    def showDefTreeName(defTree: DefTree): String
    def showFlags(tree: MemberDef): String
    def showLiteral(lit: Literal): String
    def showTypeTree(tt: TypeTree): String
    def showAttributes(tree: Tree): String  // symbol and type

    def showRefTreeName(tree: Tree): String = {
      tree match {
        case SelectFromTypeTree(qual, name) => showRefTreeName(qual) + "#" + showName(name)
        case Select(qual, name)             => showRefTreeName(qual) + "." + showName(name)
        case id @ Ident(name)               => showNameAndPos(id)
        case _                              => "" + tree
      }
    }
    def showRefTree(tree: RefTree): String = {
      def prefix0 = showRefTreeName(tree.qualifier)
      def prefix = if (prefix0 == "") "" else (tree match {
        case SelectFromTypeTree(_, _) => prefix0 + "#"
        case Select(_, _)             => prefix0 + "."
        case _                        => ""
      })
      prefix + showNameAndPos(tree) + emptyOrComment(showAttributes(tree))
    }

    def emptyOrComment(s: String) = if (s == "") "" else " // " + s

    def stringify(tree: Tree): String = {
      buf.clear()
      if (settings.XshowtreesStringified) buf.append(tree.toString + EOL)
      if (settings.XshowtreesCompact) {
        buf.append(showRaw(tree, printIds = settings.uniqid, printTypes = settings.printtypes))
      } else {
        level = 0
        traverse(tree)
      }
      buf.toString
    }
    def traverseAny(x: Any) {
      x match {
        case t: Tree      => traverse(t)
        case xs: List[_]  => printMultiline("List", "")(xs foreach traverseAny)
        case _            => println("" + x)
      }
    }
    def println(s: String) = printLine(s, "")

    def printLine(value: String, comment: String) {
      buf append "  " * level
      buf append value
      if (comment != "") {
        if (value != "")
          buf append " "

        buf append "// "
        buf append comment
      }
      buf append EOL
    }

    def annotationInfoToString(annot: AnnotationInfo): String = {
      val str = new StringBuilder
      str.append(annot.atp.toString())
      if (!annot.args.isEmpty)
        str.append(annot.args.mkString("(", ",", ")"))
      if (!annot.assocs.isEmpty)
        for (((name, value), index) <- annot.assocs.zipWithIndex) {
          if (index > 0)
            str.append(", ")
          str.append(name).append(" = ").append(value)
        }
      str.toString
    }
    def printModifiers(tree: MemberDef) {
      // SI-5885: by default this won't print annotations of not yet initialized symbols
      val annots0 = tree.symbol.annotations match {
        case Nil  => tree.mods.annotations
        case xs   => xs map annotationInfoToString
      }
      val annots = annots0 match {
        case Nil  => ""
        case xs   => " " + xs.mkString("@{ ", ", ", " }")
      }
      val flagString = showFlags(tree) match {
        case ""   => "0"
        case s    => s
      }
      println(flagString + annots)
    }

    def applyCommon(tree: Tree, fun: Tree, args: List[Tree]) {
      printMultiline(tree) {
        traverse(fun)
        traverseList("Nil", "argument")(args)
      }
    }

    def typeApplyCommon(tree: Tree, fun: Tree, args: List[Tree]) {
      printMultiline(tree) {
        traverse(fun)
        traverseList("[]", "type argument")(args)
      }
    }

    def treePrefix(tree: Tree) = showPosition(tree) + tree.productPrefix
    def printMultiline(tree: Tree)(body: => Unit) {
      printMultiline(treePrefix(tree), showAttributes(tree))(body)
    }
    def printMultiline(prefix: String, comment: String)(body: => Unit) {
      printLine(prefix + "(", comment)
      indent(body)
      println(")")
    }

    @inline private def indent[T](body: => T): T = {
      level += 1
      try body
      finally level -= 1
    }

    def traverseList(ifEmpty: String, what: String)(trees: List[Tree]) {
      if (trees.isEmpty)
        println(ifEmpty)
      else if (trees.tail.isEmpty)
        traverse(trees.head)
      else {
        printLine("", trees.length + " " + what + "s")
        trees foreach traverse
      }
    }

    def printSingle(tree: Tree, name: Name) {
      println(treePrefix(tree) + "(" + showName(name) + ")" + showAttributes(tree))
    }

    def traverse(tree: Tree) {
      showPosition(tree)

      tree match {
        case ApplyDynamic(fun, args)      => applyCommon(tree, fun, args)
        case Apply(fun, args)             => applyCommon(tree, fun, args)

        case TypeApply(fun, args)         => typeApplyCommon(tree, fun, args)
        case AppliedTypeTree(tpt, args)   => typeApplyCommon(tree, tpt, args)

        case Throw(Ident(name)) =>
          printSingle(tree, name)

        case b @ Bind(name, body) =>
          printMultiline(tree) {
            println(showDefTreeName(b))
            traverse(body)
          }

        case ld @ LabelDef(name, params, rhs) =>
          printMultiline(tree) {
            showNameAndPos(ld)
            traverseList("()", "params")(params)
            traverse(rhs)
          }

        case Function(vparams, body) =>
          printMultiline(tree) {
            traverseList("()", "parameter")(vparams)
            traverse(body)
          }
        case Try(block, catches, finalizer) =>
          printMultiline(tree) {
            traverse(block)
            traverseList("{}", "case")(catches)
            if (finalizer ne EmptyTree)
              traverse(finalizer)
          }

        case Match(selector, cases) =>
          printMultiline(tree) {
            traverse(selector)
            traverseList("", "case")(cases)
          }
        case CaseDef(pat, guard, body) =>
          printMultiline(tree) {
            traverse(pat)
            if (guard ne EmptyTree)
              traverse(guard)
            traverse(body)
          }
        case Block(stats, expr) =>
          printMultiline(tree) {
            traverseList("{}", "statement")(stats)
            traverse(expr)
          }
        case cd @ ClassDef(mods, name, tparams, impl) =>
          printMultiline(tree) {
            printModifiers(cd)
            println(showDefTreeName(cd))
            traverseList("[]", "type parameter")(tparams)
            traverse(impl)
          }
        case md @ ModuleDef(mods, name, impl) =>
          printMultiline(tree) {
            printModifiers(md)
            println(showDefTreeName(md))
            traverse(impl)
          }
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          printMultiline(tree) {
            printModifiers(dd)
            println(showDefTreeName(dd))
            traverseList("[]", "type parameter")(tparams)
            vparamss match {
              case Nil        => println("Nil")
              case ListOfNil  => println("List(Nil)")
              case ps  :: Nil =>
                printLine("", "1 parameter list")
                ps foreach traverse
              case pss        =>
                printLine("", pss.length + " parameter lists")
                pss foreach (ps => traverseList("()", "parameter")(ps))
            }
            traverse(tpt)
            traverse(rhs)
          }
        case EmptyTree =>
          println(showName(nme.EMPTY))
        case lit @ Literal(value) =>
          println(showLiteral(lit))
        case New(tpt) =>
          printMultiline(tree)(traverse(tpt))
        case Super(This(qual), mix) =>
          println("Super(This(" + showName(qual) + "), " + showName(mix) + ")")
        case Super(qual, mix) =>
          printMultiline(tree) {
            traverse(qual)
            showName(mix)
          }
        case Template(parents, self, body) =>
          printMultiline(tree) {
            val ps0 = parents map { p =>
              if (p.tpe eq null) p match {
                case x: RefTree => showRefTree(x)
                case x          => showPosition(x) + x
              }
              else showName(newTypeName(p.tpe.typeSymbol.fullName))
            }
            printLine(ps0 mkString ", ", "parents")
            traverse(self)
            traverseList("{}", "statement")(body)
          }
        case This(qual) =>
          printSingle(tree, qual)
        case tt @ TypeTree() =>
          println(showTypeTree(tt))

        case Typed(expr, tpt) =>
          printMultiline(tree) {
            traverse(expr)
            traverse(tpt)
          }
        case vd @ ValDef(mods, name, tpt, rhs) =>
          printMultiline(tree) {
            printModifiers(vd)
            println(showDefTreeName(vd))
            traverse(tpt)
            traverse(rhs)
          }
        case td @ TypeDef(mods, name, tparams, rhs) =>
          printMultiline(tree) {
            printModifiers(td)
            println(showDefTreeName(td))
            traverseList("[]", "type parameter")(tparams)
            traverse(rhs)
          }

        case PackageDef(pid, stats) =>
          printMultiline("PackageDef", "")(pid :: stats foreach traverse)

        case _ =>
          tree match {
            case t: RefTree               => println(showRefTree(t))
            case t if t.productArity == 0 => println(treePrefix(t))
            case t                        => printMultiline(tree)(tree.productIterator foreach traverseAny)
          }
      }
    }
  }

  def printUnit(unit: CompilationUnit) {
    print("// Scala source: " + unit.source + "\n")
    println(Option(unit.body) map (x => nodeToString(x) + "\n") getOrElse "<null>")
  }

  def printAll() {
    print("[[syntax trees at end of " + phase + "]]")
    global.currentRun.units foreach printUnit
  }
}
