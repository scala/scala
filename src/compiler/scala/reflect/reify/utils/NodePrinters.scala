/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.reify
package utils

import scala.compat.Platform.EOL

trait NodePrinters {
  self: Utils =>

  import global._
  import definitions._
  import Flag._

  object reifiedNodeToString extends (Tree => String) {
    // [Eugene++ to Martin] can we do better?
    // didn't want to invent anything myself in order not to interfere with your line of thought
    def bitsToFlags(bits: String): String = {
      val flags = bits.toLong
      if (flags == NoFlags) nme.NoFlags.toString
      else {
        val s_flags = new collection.mutable.ListBuffer[String]
        if (flags containsAll TRAIT) s_flags += "TRAIT"
        if (flags containsAll MODULE) s_flags += "MODULE"
        if (flags containsAll MUTABLE) s_flags += "MUTABLE"
        if (flags containsAll PACKAGE) s_flags += "PACKAGE"
        if (flags containsAll METHOD) s_flags += "METHOD"
        if (flags containsAll DEFERRED) s_flags += "DEFERRED"
        if (flags containsAll ABSTRACT) s_flags += "ABSTRACT"
        if (flags containsAll FINAL) s_flags += "FINAL"
        if (flags containsAll SEALED) s_flags += "SEALED"
        if (flags containsAll IMPLICIT) s_flags += "IMPLICIT"
        if (flags containsAll LAZY) s_flags += "LAZY"
        if (flags containsAll OVERRIDE) s_flags += "OVERRIDE"
        if (flags containsAll PRIVATE) s_flags += "PRIVATE"
        if (flags containsAll PROTECTED) s_flags += "PROTECTED"
        if (flags containsAll CASE) s_flags += "CASE"
        if (flags containsAll ABSOVERRIDE) s_flags += "ABSOVERRIDE"
        if (flags containsAll BYNAMEPARAM) s_flags += "BYNAMEPARAM"
        if (flags containsAll PARAM) s_flags += "PARAM"
        if (flags containsAll PARAMACCESSOR) s_flags += "PARAMACCESSOR"
        if (flags containsAll CASEACCESSOR) s_flags += "CASEACCESSOR"
        if (flags containsAll COVARIANT) s_flags += "COVARIANT"
        if (flags containsAll CONTRAVARIANT) s_flags += "CONTRAVARIANT"
        if (flags containsAll DEFAULTPARAM) s_flags += "DEFAULTPARAM"
        if (flags containsAll INTERFACE) s_flags += "INTERFACE"
        s_flags mkString " | "
      }
    }

    def apply(tree: Tree): String = {
      var mirrorIsUsed = false
      var flagsAreUsed = false

      // @PP: I fervently hope this is a test case or something, not anything being
      // depended upon.  Of more fragile code I cannot conceive.
      // @Eugene: This stuff is only needed to debug-print out reifications in human-readable format
      // Rolling a full-fledged, robust TreePrinter would be several times more code.
      val lines = (tree.toString.split(EOL) drop 1 dropRight 1).toList splitAt 2
      var (List(universe, mirror), reification) = lines
      reification = (for (line <- reification) yield {
        var s = line substring 2
        s = s.replace(nme.UNIVERSE_PREFIX.toString, "")
        s = s.replace(".apply", "")
        s = "([^\"])scala\\.collection\\.immutable\\.".r.replaceAllIn(s, "$1")
        s = "List\\[List\\[.*?\\].*?\\]".r.replaceAllIn(s, "List")
        s = "List\\[.*?\\]".r.replaceAllIn(s, "List")
        s = s.replace("immutable.this.Nil", "List()")
        s = """build\.flagsFromBits\((\d+)[lL]\)""".r.replaceAllIn(s, m => {
          flagsAreUsed = true
          bitsToFlags(m.group(1))
        })
        s = s.replace("Modifiers(0L, newTypeName(\"\"), List())", "Modifiers()")
        s = """Modifiers\((\d+)[lL], newTypeName\("(.*?)"\), List\((.*?)\)\)""".r.replaceAllIn(s, m => {
          val buf = new collection.mutable.ListBuffer[String]

          val annotations = m.group(3)
          if (buf.nonEmpty || annotations.nonEmpty)
            buf.append("List(" + annotations + ")")

          val privateWithin = "" + m.group(2)
          if (buf.nonEmpty || privateWithin != "")
            buf.append("newTypeName(\"" + privateWithin + "\")")

          val bits = m.group(1)
          if (buf.nonEmpty || bits != "0L") {
            flagsAreUsed = true
            buf.append(bitsToFlags(bits))
          }

          val replacement = "Modifiers(" + buf.reverse.mkString(", ")  + ")"
          java.util.regex.Matcher.quoteReplacement(replacement)
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

      val printout = collection.mutable.ListBuffer[String]();
      printout += universe.trim
      if (mirrorIsUsed) printout += mirror.replace("MirrorOf[", "scala.reflect.base.MirrorOf[").trim
      val imports = collection.mutable.ListBuffer[String]();
      imports += nme.UNIVERSE_SHORT
      // if (buildIsUsed) imports += nme.build
      if (mirrorIsUsed) imports += nme.MIRROR_SHORT
      if (flagsAreUsed) imports += nme.Flag
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
          printout += s"println(${nme.MIRROR_SHORT}.mkToolBox().runExpr(tree))"
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
