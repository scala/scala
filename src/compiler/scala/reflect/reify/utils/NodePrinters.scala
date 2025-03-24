/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.reify
package utils

import java.lang.System.{lineSeparator => EOL}
import scala.util.matching.Regex.quoteReplacement

trait NodePrinters {
  self: Utils =>

  import global._

  object reifiedNodeToString extends (Tree => String) {
    def apply(tree: Tree): String = {
      var mirrorIsUsed = false
      var flagsAreUsed = false

      // @PP: I fervently hope this is a test case or something, not anything being
      // depended upon.  Of more fragile code I cannot conceive.
      // @Eugene: This stuff is only needed to debug-print out reifications in human-readable format
      // Rolling a full-fledged, robust TreePrinter would be several times more code.
      // Also as of late we have tests that ensure that UX won't be broken by random changes to the reifier.
      val lines = (tree.toString.split(EOL) drop 1 dropRight 1).toList splitAt 2
      val (List(universe, mirror), reification0) = lines: @unchecked
      val reification = (for (line <- reification0) yield {
        var s = line substring 2
        s = s.replace(nme.UNIVERSE_PREFIX.toString, "")
        s = s.replace(".apply", "")
        s = "([^\"])(_root_\\.)?scala\\.collection\\.immutable\\.".r.replaceAllIn(s, "$1")
        s = "List\\[List\\[.*?\\].*?\\]".r.replaceAllIn(s, "List")
        s = "List\\[.*?\\]".r.replaceAllIn(s, "List")
        s = s.replace("immutable.this.Nil", "List()")
        s = """internal\.reificationSupport\.FlagsRepr\((\d+)[lL]\)""".r.replaceAllIn(s, m => {
          flagsAreUsed = true
          show(m.group(1).toLong)
        })
        s = s.replace("Modifiers(0L, TypeName(\"\"), List())", "Modifiers()")
        s = """Modifiers\((\d+)[lL], TypeName\("(.*?)"\), List\((.*?)\)\)""".r.replaceAllIn(s, m => {
          val buf = new scala.collection.mutable.ListBuffer[String]

          val annotations = m.group(3)
          if (buf.nonEmpty || annotations != "")
            buf += s"List($annotations)"

          val privateWithin = "" + m.group(2)
          if (buf.nonEmpty || privateWithin != "")
            buf += {"TypeName(\"" + privateWithin + "\")"}

          val bits = m.group(1)
          if (buf.nonEmpty || bits != "0L") {
            flagsAreUsed = true
            buf += show(bits.toLong)
          }

          val replacement = "Modifiers(" + buf.reverse.mkString(", ")  + ")"
          quoteReplacement(replacement)
        })
        s
      })

      val isExpr = reification.length > 0 && reification(0).trim.startsWith("Expr[")
      var rtree = reification dropWhile (!_.trim.startsWith(s"val ${nme.UNIVERSE_SHORT}: U = ${nme.MIRROR_UNTYPED}.universe;"))
      rtree = rtree drop 2
      rtree = rtree takeWhile (_ != "    }")
      rtree = rtree map (s0 => {
        var s = s0
        mirrorIsUsed |= s contains nme.MIRROR_PREFIX.toString
        s = s.replace(nme.MIRROR_PREFIX.toString, "")
        s.trim
      })

      val printout = scala.collection.mutable.ListBuffer[String]()
      printout += universe.trim
      if (mirrorIsUsed) printout += mirror.replace("Mirror[", "scala.reflect.api.Mirror[").trim
      val imports = scala.collection.mutable.ListBuffer[String]()
      imports += nme.UNIVERSE_SHORT.toString
      if (mirrorIsUsed) imports += nme.MIRROR_SHORT.toString
      if (flagsAreUsed) imports += nme.Flag.toString
      printout += s"""import ${imports map (_ + "._") mkString ", "}"""

      val name = if (isExpr) "tree" else "tpe"
      if (rtree(0) startsWith "val") {
        printout += s"val $name = {"
        printout ++= (rtree map ("  " + _))
        printout += "}"
      } else {
        printout += s"val $name = " + rtree(0)
      }
      if (isExpr) {
        if (mirror contains ".getClassLoader") {
          printout += "import scala.tools.reflect.ToolBox"
          printout += s"println(${nme.MIRROR_SHORT}.mkToolBox().eval(tree))"
        } else {
          printout += "println(tree)"
        }
      } else {
        printout += "println(tpe)"
      }

      // printout mkString EOL
      val prefix = "// produced from " + reifier.defaultErrorPosition
      (prefix +: "object Test extends App {" +: (printout map ("  " + _)) :+ "}") mkString EOL
    }
  }
}
