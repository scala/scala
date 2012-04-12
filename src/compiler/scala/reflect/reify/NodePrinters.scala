/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package reify

import scala.Array.canBuildFrom
import scala.compat.Platform.EOL
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.Global

trait NodePrinters { self: scala.tools.nsc.ast.NodePrinters =>

  val global: Global
  import global._

  object reifiedNodeToString extends Function2[Tree, Tree, String] {
    def apply(prefix: Tree, tree: Tree): String = {
      import scala.reflect.api.Modifier
      var modifierIsUsed = false
      var flagsAreUsed = false

      // @PP: I fervently hope this is a test case or something, not anything being
      // depended upon.  Of more fragile code I cannot conceive.
      // @Eugene: This stuff is only needed to debug-print out reifications in human-readable format
      // Rolling a full-fledged, robust TreePrinter would be several times more code.
      val (List(mirror), reified) = (for (line <- (tree.toString.split(EOL).toList drop 1 dropRight 1)) yield {
        var s = line.trim
        s = s.replace("$mr.", "")
        s = s.replace(".apply", "")
        s = s.replace("scala.collection.immutable.", "")
        s = "List\\[List\\[.*?\\].*?\\]".r.replaceAllIn(s, "List")
        s = "List\\[.*?\\]".r.replaceAllIn(s, "List")
        s = s.replace("immutable.this.Nil", "List()")
        s = s.replace("modifiersFromInternalFlags", "Modifiers")
        s = s.replace("Modifiers(0L, newTypeName(\"\"), List())", "Modifiers()")
        s = """Modifiers\((\d+)[lL], newTypeName\("(.*?)"\), List\((.*?)\)\)""".r.replaceAllIn(s, m => {
          val buf = new collection.mutable.ListBuffer[String]

          val annotations = m.group(3)
          if (buf.nonEmpty || annotations.nonEmpty)
            buf.append("List(" + annotations + ")")

          val privateWithin = "" + m.group(2)
          if (buf.nonEmpty || privateWithin != "")
            buf.append("newTypeName(\"" + privateWithin + "\")")

          val flags = m.group(1).toLong
          val s_flags = Flags.modifiersOfFlags(flags) map (_.sourceString) mkString ", "
          if (buf.nonEmpty || s_flags != "") {
            modifierIsUsed = true
            buf.append("Set(" + s_flags + ")")
          }

          "Modifiers(" + buf.reverse.mkString(", ")  + ")"
        })
        s = """setInternalFlags\((\d+)L\)""".r.replaceAllIn(s, m => {
          flagsAreUsed = true
          val flags = m.group(1).toLong
          val mods = Flags.modifiersOfFlags(flags) map (_.sourceString)
          "setInternalFlags(flagsOfModifiers(List(" + mods.mkString(", ") + ")))"
        })

        s
      }) splitAt 1

      val printout = collection.mutable.ListBuffer(mirror);
      printout += "import " + nme.MIRROR_SHORT + "._"
      if (modifierIsUsed) printout += "import scala.reflect.api.Modifier._"
      if (flagsAreUsed) printout += "import scala.reflect.internal.Flags._"
      val body = reified dropWhile (_.startsWith("val"))
      if (body.length > 0 && body(0).startsWith("Expr[")) {
        if (reified(0) startsWith "val") {
          printout += "val code = {"
          printout ++= (reified map ("  " + _))
          printout += "}"
          printout += "mkToolBox().runExpr(code)"
        } else {
          printout += "val code = " + reified(0)
          printout ++= reified drop 1
          printout += "mkToolBox().runExpr(code)"
        }
        try {
          val prefix = Select(Select(Ident(definitions.ScalaPackage), newTermName("reflect")), newTermName("mirror"))
          val tree1 = new global.Transformer {
            override def transform(tree: Tree) = super.transform(tree match {
              case Block(ValDef(_, mr, _, _) :: Nil, expr) if mr == nme.MIRROR_SHORT => transform(expr)
              case Block(ValDef(_, mr, _, _) :: symbolTable, expr) if mr == nme.MIRROR_SHORT => transform(Block(symbolTable, expr))
              case Select(Ident(mr), name) if mr == nme.MIRROR_SHORT => Select(prefix, name)
              case tree => tree
            })
          }.transform(tree)
          val stringified = mkToolBox().runExpr(tree1).toString
          if (settings.Yreifydebug.value) printout += "*****************************"
          printout += stringified
        } catch {
          case ex: Throwable =>
//            val realex = ReflectionUtils.unwrapThrowable(ex)
//            val message = new java.io.StringWriter()
//            realex.printStackTrace(new java.io.PrintWriter(message))
//            println(message)
        }
      } else {
        printout ++= reified
      }
      printout mkString EOL
    }
  }
}