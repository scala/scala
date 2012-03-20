/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import compat.Platform.EOL
import symtab.Flags._

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

  def nodeToString: Tree => String =
    if (sys.props contains "scala.colors") nodeToColorizedString
    else nodeToRegularString
  
  object nodeToRegularString extends DefaultPrintAST with (Tree => String) {
    def apply(tree: Tree) = stringify(tree)
  }
  
  object nodeToColorizedString extends ColorPrintAST with (Tree => String) {
    def apply(tree: Tree) = stringify(tree)
  }

  trait ColorPrintAST extends DefaultPrintAST {
    import scala.tools.util.color._
    
    def keywordColor = Cyan
    def typeColor    = Yellow
    def termColor    = Blue
    def flagColor    = Red
    def literalColor = Green
    
    override def showFlags(tree: MemberDef) =
      super.showFlags(tree) in flagColor.bright

    override def showDefTreeName(tree: DefTree) =
      if (tree.name.isTermName) tree.name.decode in termColor.bright
      else tree.name.decode in typeColor.bright

    override def showName(name: Name) =
      if (name == nme.EMPTY || name == tpnme.EMPTY) "<empty>" in keywordColor
      else if (name.isTermName) name.decode in termColor
      else name.decode in typeColor

    override def showLiteral(lit: Literal) =
      super.showLiteral(lit) in literalColor.bright
  }

  trait DefaultPrintAST extends PrintAST {
    def showDefTreeName(tree: DefTree) = showName(tree.name)
    def showFlags(tree: MemberDef)     = flagsToString(tree.symbol.flags | tree.mods.flags)
    def showLiteral(lit: Literal)      = lit.value.escapedStringValue
    def showTypeTree(tt: TypeTree)     = "<tpt>" + showAttributes(tt)
    def showName(name: Name)           = name match {
      case nme.EMPTY | tpnme.EMPTY => "<empty>"
      case name                    => "\"" + name + "\""
    }

    def showSymbol(tree: Tree): String = {
      val sym = tree.symbol
      if (sym == null || sym == NoSymbol) ""
      else " sym/owner/tpe=%s %s/%s/%s".format(sym.accurateKindString, sym.name, sym.owner, sym.tpe)
    }
    def showType(tree: Tree): String = {
      val tpe = tree.tpe
      if (tpe == null || tpe == NoType) ""
      else " tree.tpe=" + tpe
    }
  
    def showAttributes(tree: Tree): String = {
      if (infolevel == InfoLevel.Quiet) ""
      else {
        try   { showSymbol(tree) + showType(tree) trim }
        catch { case ex: Throwable => "sym= <error> " + ex.getMessage }
      }
    }
  }
  
  trait PrintAST {
    private val buf = new StringBuilder
    private var level = 0

    def showName(name: Name): String
    def showDefTreeName(defTree: DefTree): String
    def showFlags(tree: MemberDef): String
    def showLiteral(lit: Literal): String
    def showTypeTree(tt: TypeTree): String
    def showAttributes(tree: Tree): String  // symbol and type
    
    def showRefTreeName(tree: Tree): String = tree match {
      case SelectFromTypeTree(qual, name) => showRefTreeName(qual) + "#" + showName(name)
      case Select(qual, name)             => showRefTreeName(qual) + "." + showName(name)
      case Ident(name)                    => showName(name)
      case _                              => "" + tree
    }
    def showRefTree(tree: RefTree): String = {
      def prefix0 = showRefTreeName(tree.qualifier)
      def prefix = if (prefix0 == "") "" else (tree match {
        case SelectFromTypeTree(_, _) => prefix0 + "#"
        case Select(_, _)             => prefix0 + "."
        case _                        => ""
      })
      def attrs = showAttributes(tree) match {
        case "" => ""
        case s  => " // " + s
      }
      prefix + showName(tree.name) + attrs
    }

    def stringify(tree: Tree): String = {
      buf.clear()
      level = 0
      traverse(tree)
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
        buf append " // "
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
        traverseList("Nil", _ + " arguments(s)")(args)
      }
    }
    
    def printMultiline(tree: Tree)(body: => Unit) {
      printMultiline(tree.printingPrefix, showAttributes(tree))(body)
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

    def traverseList(ifEmpty: String, comment: Int => String)(trees: List[Tree]) {
      if (trees.isEmpty)
        println(ifEmpty)
      else
        printMultiline("List", comment(trees.length))(trees foreach traverse)
    }

    def traverse(tree: Tree) {
      tree match {
        case AppliedTypeTree(tpt, args) => applyCommon(tree, tpt, args)
        case ApplyDynamic(fun, args)    => applyCommon(tree, fun, args)
        case Apply(fun, args)           => applyCommon(tree, fun, args)

        case Block(stats, expr) =>
          printMultiline(tree) {
            traverseList("{}", _ + " statement(s)")(stats)
            traverse(expr)
          }
        case cd @ ClassDef(mods, name, tparams, impl) =>
          printMultiline(tree) {
            printModifiers(cd)
            println(showDefTreeName(cd))
            traverseList("[]", _ + " type parameter(s)")(tparams)
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
            traverseList("[]", _ + " type parameter(s)")(tparams)
            vparamss match {
              case Nil        => println("Nil")
              case Nil :: Nil => println("List(Nil)")
              case xss        =>
                printMultiline("List", xss.length + " parameter list(s)") {
                  xss foreach (xs => traverseList("()", _ + " parameter(s)")(xs))
                }
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
                case x          => "" + x
              }
              else showName(newTypeName(p.tpe.typeSymbol.fullName))
            }
            printLine(ps0 mkString ", ", "parents")
            traverse(self)
            traverseList("{}", _ + " statements in body")(body)
          }
        case This(qual) =>
          println("This(\"" + showName(qual) + "\")" + showAttributes(tree))
        case TypeApply(fun, args) =>
          printMultiline(tree) {
            traverse(fun)
            traverseList("[]", _ + " type argument(s)")(args)
          }
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
            traverseList("[]", _ + " type parameter(s)")(tparams)
            traverse(rhs)
          }
        
        case PackageDef(pid, stats) =>
          printMultiline("PackageDef", "")(pid :: stats foreach traverse)

        case _ =>
          tree match {
            case t: RefTree               => println(showRefTree(t))
            case t if t.productArity == 0 => println(tree.printingPrefix)
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
