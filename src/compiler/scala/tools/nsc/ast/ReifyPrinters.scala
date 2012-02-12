/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import compat.Platform.EOL
import symtab._
import Flags._

trait ReifyPrinters { self: NodePrinters =>

  val global: Global
  import global._

  object reifiedNodeToString extends Function1[Tree, String] {
    def apply(tree: Tree): String = {
      import scala.reflect.api.Modifier

      // @PP: I fervently hope this is a test case or something, not anything being
      // depended upon.  Of more fragile code I cannot conceive.
      // @eb: This stuff is only needed to debug-print out reifications in human-readable format
      // Rolling a full-fledged, robust TreePrinter would be several times more code.
      (for (line <- (tree.toString.split(EOL) drop 2 dropRight 1)) yield {
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
          if (buf.nonEmpty || s_flags != "")
            buf.append("Set(" + s_flags + ")")

          "Modifiers(" + buf.reverse.mkString(", ")  + ")"
        })
        s = """setInternalFlags\((\d+)L\)""".r.replaceAllIn(s, m => {
          val flags = m.group(1).toLong
          val mods = Flags.modifiersOfFlags(flags) map (_.sourceString)
          "setInternalFlags(flagsOfModifiers(List(" + mods.mkString(", ") + ")))"
        })

        s
      }) mkString EOL
    }
  }


  def printReifyCopypaste(tree: Tree) {
    val reifyDebug = settings.Yreifydebug.value
    if (reifyDebug) println("=======================")
    printReifyCopypaste1(tree)
    if (reifyDebug) println("=======================")
  }

  def printReifyCopypaste1(tree: Tree) {
  }
}