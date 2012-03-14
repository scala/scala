/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

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
        def symflags(tree: Tree): String = {
          val buf = new StringBuffer
          val sym = tree.symbol
          buf append flagsToString(sym.flags)

          val annots = ", annots=" + (
            if (!sym.annotations.isEmpty)
              sym.annotations.map(annotationInfoToString).mkString("[", ",", "]")
            else
              tree.asInstanceOf[MemberDef].mods.annotations)
          (if (buf.length() > 2) buf.substring(3)
          else "0") + ", // flags=" + flagsToString(sym.flags) + annots
        }

        def nodeinfo(tree: Tree): String =
          if (infolevel == InfoLevel.Quiet) ""
          else {
            try {
              val buf = new StringBuilder(" // sym=" + tree.symbol)
              if (tree.hasSymbol) {
                if (tree.symbol.isPrimaryConstructor)
                  buf.append(", isPrimaryConstructor")
                else if (tree.symbol.isConstructor)
                  buf.append(", isConstructor")
                if (tree.symbol != NoSymbol)
                  buf.append(", sym.owner=" + tree.symbol.owner)
                buf.append(", sym.tpe=" + tree.symbol.tpe)
              }
              buf.append(", tpe=" + tree.tpe)
              if (tree.tpe != null) {
                var sym = tree.tpe.termSymbol
                if (sym == NoSymbol) sym = tree.tpe.typeSymbol
                buf.append(", tpe.sym=" + sym)
                if (sym != NoSymbol) {
                  buf.append(", tpe.sym.owner=" + sym.owner)
                  if ((infolevel > InfoLevel.Normal) &&
                      !(sym.owner eq definitions.ScalaPackageClass) &&
                      !sym.isModuleClass && !sym.isPackageClass &&
                      !sym.isJavaDefined) {
                    val members = for (m <- tree.tpe.decls)
                      yield m.toString() + ": " + m.tpe + ", "
                    buf.append(", tpe.decls=" + members)
                  }
                }
              }
              buf.toString
            } catch {
              case ex: Throwable =>
                return " // sym= <error> " + ex.getMessage
            }
          }
        def nodeinfo2(tree: Tree): String =
          (if (comma) "," else "") + nodeinfo(tree)

        def applyCommon(name: String, tree: Tree, fun: Tree, args: List[Tree]) {
          println(name + "(" + nodeinfo(tree))
          traverse(fun, level + 1, true)
          if (args.isEmpty)
            println("  Nil // no argument")
          else {
            val n = args.length
            println("  List( // " + n + " arguments(s)")
            for (i <- 0 until n)
              traverse(args(i), level + 2, i < n-1)
            println("  )")
          }
          printcln(")")
        }

        tree match {
          case AppliedTypeTree(tpt, args) => applyCommon("AppliedTypeTree", tree, tpt, args)
          case Apply(fun, args)           => applyCommon("Apply", tree, fun, args)
          case ApplyDynamic(fun, args)    => applyCommon("ApplyDynamic", tree, fun, args)

          case Block(stats, expr) =>
            println("Block(" + nodeinfo(tree))
            if (stats.isEmpty)
              println("  List(), // no statement")
            else {
              val n = stats.length
              println("  List( // " + n + " statement(s)")
              for (i <- 0 until n)
                traverse(stats(i), level + 2, i < n-1)
              println("  ),")
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
            val n = vparamss.length
            if (n == 1 && vparamss(0).isEmpty)
              println("  List(List()), // no parameter")
            else {
              println("  List(")
              for (i <- 0 until n) {
                val m = vparamss(i).length
                println("    List( // " + m + " parameter(s)")
                for (j <- 0 until m)
                  traverse(vparamss(i)(j), level + 3, j < m-1)
                println("    )")
              }
              println("  ),")
            }
            println("  " + tpt + ",")
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
            println("Super(\"" + mix + "\")" + nodeinfo(tree))
            traverse(qual, level + 1, true)
          case Template(parents, self, body) =>
            println("Template(" + nodeinfo(tree))
            println("  " + parents.map(p =>
                if (p.tpe ne null) p.tpe.typeSymbol else "null-" + p
              ) + ", // parents")
            traverse(self, level + 1, true)
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
          case PackageDef(pid, stats) =>
            println("PackageDef(")
            traverse(pid, level + 1, false)
            println(",\n")
            for (stat <- stats)
              traverse(stat, level + 1, false)
            printcln(")")
          case _ =>
            tree match {
              case p: Product =>
                if (p.productArity != 0) {
                  println(p.productPrefix+"(")
                  for (elem <- (0 until p.productArity) map p.productElement) {
                    def printElem(elem: Any, level: Int): Unit = elem match {
                      case t: Tree =>
                        traverse(t, level, false)
                      case xs: List[_] =>
                        print("List(")
                        for (x <- xs) printElem(x, level+1)
                        printcln(")")
                      case _ =>
                        println(elem.toString)
                    }
                    printElem(elem, level+1)
                  }
                  printcln(")")
                } else printcln(p.productPrefix)
            }
        }
      }
      buf setLength 0
      traverse(tree, 0, false)
      buf.toString
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
