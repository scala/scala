/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: $

package scala.tools.nsc.ast

import compat.Platform.EOL
import symtab.Flags._

/** The object <code>nodePrinter</code> converts the internal tree
 *  representation to a string formatted as a Scala expression.
 *
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
abstract class NodePrinters {

  val global: Global
  import global._

  object InfoLevel extends Enumeration {
    val Quiet, Normal, Verbose = Value
  }
  var infolevel = InfoLevel.Quiet

  object nodeToString extends Function1[Tree, String] {
    private val buf = new StringBuilder

    def apply(tree: Tree): String = {
      def traverse(tree: Tree, level: Int, comma: Boolean) {
        def println(s: String) {
          for (i <- 0 until level) buf.append("  ")
          buf.append(s)
          buf.append(EOL)
        }
        def printcln(s: String) {
          for (i <- 0 until level) buf.append("  ")
          buf.append(s)
          if (comma) buf.append(",")
          buf.append(EOL)
        }
        def annotationInfoToString(attr: AnnotationInfo[Constant]) = {
          val str = new StringBuilder
          str.append(attr.atp.toString())
          if (!attr.args.isEmpty)
            str.append(attr.args.mkString("(", ",", ")"))
          if (!attr.assocs.isEmpty)
            for (((name, value), index) <- attr.assocs.zipWithIndex) {
              if (index > 0)
                str.append(", ")
              str.append(name).append(" = ").append(value)
            }
          str.toString
        }
        def symflags(tree: Tree): String = {
          val sym = tree.symbol
          val buf = new StringBuffer
          if (sym hasFlag BRIDGE       ) buf.append(" | BRIDGE")
          if (sym hasFlag FINAL        ) buf.append(" | FINAL")
          if (sym hasFlag LOCAL        ) buf.append(" | LOCAL")
          if (sym hasFlag METHOD       ) buf.append(" | METHOD")
          if (sym hasFlag PARAMACCESSOR) buf.append(" | PARAMACCESSOR")
          if (sym hasFlag PRIVATE      ) buf.append(" | PRIVATE")
          if (sym hasFlag STATIC       ) buf.append(" | STATIC")
          if (sym hasFlag SYNTHETIC    ) buf.append(" | SYNTHETIC")
          val attrs = ", attrs=" + (
            if (!sym.attributes.isEmpty)
              sym.attributes.map(annotationInfoToString).mkString("[", ",", "]")
            else
              tree.asInstanceOf[MemberDef].mods.annotations)
          (if (buf.length() > 2) buf.substring(3)
          else "0") + ", // flags=" + flagsToString(sym.flags) + attrs
        }
        def nodeinfo(tree: Tree) =
          if (infolevel == InfoLevel.Quiet) ""
          else {
            val buf = new StringBuilder(" // sym=" + tree.symbol)
            if (infolevel > InfoLevel.Quiet && tree.symbol != null)
              buf.append(", sym.tpe=" + tree.symbol.tpe)
            if (tree.hasSymbol)
              if (tree.symbol.isPrimaryConstructor)
                buf.append(", isPrimaryConstructor")
              else if (tree.symbol.isConstructor)
                buf.append(", isConstructor")
            buf.append(", tpe=" + tree.tpe)
            if (tree.tpe != null) {
              val sym = tree.tpe.symbol
              buf.append(", tpe.sym=" + sym)
              if (sym != NoSymbol) {
                buf.append(", tpe.sym.owner=" + sym.owner)
                if ((infolevel > InfoLevel.Normal) &&
                    !(sym.owner eq definitions.ScalaPackageClass) &&
                    !sym.isModuleClass && !sym.isPackageClass &&
                    !sym.hasFlag(JAVA)) {
                  val members = for (m <- tree.tpe.decls.toList)
                    yield m.toString() + ": " + m.tpe + ", "
                  buf.append(", tpe.decls=" + members)
                }
              }
            }
            buf.toString
          }
        def nodeinfo2(tree: Tree) =
          (if (comma) "," else "") + nodeinfo(tree)
        tree match {
          case AppliedTypeTree(tpt, args) =>
            println("AppliedTypeTree(" + nodeinfo(tree))
            traverse(tpt, level + 1, true)
            if (args.isEmpty)
              println("  List() // no argument")
            else {
              val n = args.length
              println("  List( // " + n + " arguments(s)")
              for (i <- 0 until n)
                traverse(args(i), level + 2, i < n-1)
              println("  )")
            }
            printcln(")")
          case Apply(fun, args) =>
            println("Apply(" + nodeinfo(tree))
            traverse(fun, level + 1, true)
            if (args.isEmpty)
              println("  List() // no argument")
            else {
              val n = args.length
              println("  List( // " + n + " argument(s)")
              for (i <- 0 until n)
                traverse(args(i), level + 2, i < n-1)
              println("  )")
            }
            printcln(")")
          case Block(stats, expr) =>
            println("Block(")
            if (stats.isEmpty)
              println("  List() // no statement")
            else {
              val n = stats.length
              println("  List( // " + n + " statement(s)")
              for (i <- 0 until n)
                traverse(stats(i), level + 2, i < n-1)
              println("  )")
            }
            traverse(expr, level + 1, false)
            printcln(")")
          case ClassDef(mods, name, tparams, impl) =>
            println("ClassDef(" + nodeinfo(tree))
            println("  " + symflags(tree))
            println("  \"" + name + "\",")
            if (tparams.isEmpty)
              println("  List(), // no type parameter")
            else {
              val n = tparams.length
              println("  List( // " + n + " type parameter(s)")
              for (i <- 0 until n)
                traverse(tparams(i), level + 2, i < n-1)
              println("  ),")
            }
            traverse(impl, level + 1, false)
            printcln(")")
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            println("DefDef(" + nodeinfo(tree))
            println("  " + symflags(tree))
            println("  \"" + name + "\",")
            if (tparams.isEmpty)
              println("  List(), // no type parameter")
            else {
              val n = tparams.length
              println("  List( // " + n + " type parameter(s)")
              for (i <- 0 until n)
                traverse(tparams(i), level + 2, i < n-1)
              println("  ),")
            }
            traverse(rhs, level + 1, false)
            printcln(")")
          case EmptyTree =>
            printcln("EmptyTree")
          case Ident(name) =>
            printcln("Ident(\"" + name + "\")" + nodeinfo2(tree))
          case Literal(value) =>
            printcln("Literal(" + value + ")")
          case New(tpt) =>
            println("New(" + nodeinfo(tree))
            traverse(tpt, level + 1, false)
            printcln(")")
          case Select(qualifier, selector) =>
            println("Select(" + nodeinfo(tree))
            traverse(qualifier, level + 1, true)
            printcln("  \"" + selector + "\")")
          case Super(qual, mix) =>
            printcln("Super(\"" + qual + "\", \"" + mix + "\")" + nodeinfo2(tree))
          case Template(parents, self, body) =>
            println("Template(" + nodeinfo(tree))
            println("  " + parents.map(p => p.tpe.symbol) + ", // parents")
            if (body.isEmpty)
              println("  List() // no body")
            else {
              val n = body.length
              println("  List( // body")
              for (i <- 0 until n)
                traverse(body(i), level + 2, i < n-1)
              println("  )")
            }
            printcln(")")
          case This(qual) =>
            println("This(\"" + qual + "\")" + nodeinfo2(tree))
          case TypeApply(fun, args) =>
            println("TypeApply(" + nodeinfo(tree))
            traverse(fun, level + 1, true)
            if (args.isEmpty)
              println("  List() // no argument")
            else {
              val n = args.length
              println("  List(")
              for (i <- 0 until n)
                traverse(args(i), level + 1, i < n-1)
              println("  )")
            }
            printcln(")")
          case TypeTree() =>
            printcln("TypeTree()" + nodeinfo2(tree))
          case Typed(expr, tpt) =>
            println("Typed(" + nodeinfo(tree))
            traverse(expr, level + 1, true)
            traverse(tpt, level + 1, false)
            printcln(")")
          case ValDef(mods, name, tpt, rhs) =>
            println("ValDef(" + nodeinfo(tree))
            println("  " + symflags(tree))
            println("  \"" + name + "\",")
            traverse(tpt, level + 1, true)
            traverse(rhs, level + 1, false)
            printcln(")")
          case _ =>
            printcln("***" + tree.getClass)
        }
      }
      buf setLength 0
      traverse(tree, 0, false)
      buf.toString
    }
  }
}
