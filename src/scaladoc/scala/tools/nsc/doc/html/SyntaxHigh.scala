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

package scala
package tools.nsc.doc.html

import scala.annotation.{switch, tailrec}

/** Highlight the syntax of Scala code appearing in a `{{{` wiki block
  * (see method `HtmlPage.blockToHtml`).
  *
  * @author Stephane Micheloud
  */
private[html] object SyntaxHigh {
  import HtmlTags.{Elems, Raw, NoElems}

  /** Reserved words, sorted alphabetically
    * (see [[scala.reflect.internal.StdNames]]) */
  val reserved = Array(
    "abstract", "case", "catch", "class", "def",
    "do", "else", "extends", "false", "final", "finally",
    "for", "if", "implicit", "import", "lazy", "match",
    "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield").sorted

  /** Annotations, sorted alphabetically */
  val annotations = Array(
    "BeanProperty", "BooleanBeanProperty", "SerialVersionUID",
    "beanGetter", "beanSetter",
    "companionClass", "companionMethod", "companionObject", "compileTimeOnly",
    "deprecated", "deprecatedName", "deprecatedOverriding", "deprecatedInheritance",
    "elidable", "field", "getter", "implicitAmbiguous", "implicitNotFound", "inline",
    "languageFeature",
    "migration", "native", "noinline", "param",
    "setter", "showAsInfix", "specialized", "strictfp", "switch",
    "tailrec", "throws", "transient",
    "unchecked", "uncheckedStable", "uncheckedVariance", "unspecialized",
    "varargs", "volatile").sorted

  /** Standard library classes/objects, sorted alphabetically */
  val standards = Array(
    "Any", "AnyRef", "AnyVal", "App", "Array",
    "Boolean", "Byte", "Char", "Class", "ClassManifest", "ClassTag",
    "Console", "Double", "Enumeration", "Float", "Function", "Int",
    "List", "Long", "Manifest", "Map",
    "NoManifest", "None", "Nothing", "Null", "Object", "Option", "OptManifest",
    "Predef",
    "Seq", "Set", "Short", "Some", "String", "Symbol",
    "TypeTag", "Unit", "WeakTypeTag").sorted

  def apply(data: String): Elems = {
    val buf = data.toCharArray
    val out = new StringBuilder

    def compare(offset: Int, key: String): Int = {
      var i = offset
      var j = 0
      val l = key.length
      while (i < buf.length && j < l) {
        val bch = buf(i)
        val kch = key charAt j
        if (bch < kch) return -1
        else if (bch > kch) return 1
        i += 1
        j += 1
      }
      if (j < l) -1
      else if (i < buf.length &&
               ('A' <= buf(i) && buf(i) <= 'Z' ||
                'a' <= buf(i) && buf(i) <= 'z' ||
                '0' <= buf(i) && buf(i) <= '9' ||
                buf(i) == '_')) 1
      else 0
    }

    def lookup(a: Array[String], i: Int): Int = {
      var lo = 0
      var hi = a.length - 1
      while (lo <= hi) {
        val m = (hi + lo) / 2
        val d = compare(i, a(m))
        if (d < 0) hi = m - 1
        else if (d > 0) lo = m + 1
        else return m
      }
      -1
    }

    def comment(i: Int): String = {
      val out = new StringBuilder("/")
      def line(i: Int): Int =
        if (i == buf.length || buf(i) == '\n') i
        else {
          out append buf(i)
          line(i+1)
        }
      var level = 0
      def multiline(i: Int, star: Boolean): Int = {
        if (i == buf.length) return i
        val ch = buf(i)
        out append ch
        ch match {
          case '*' =>
            if (star) level += 1
            multiline(i+1, !star)
          case '/' =>
            if (star) {
              if (level > 0) level -= 1
              if (level == 0) i else multiline(i+1, star = true)
            } else
              multiline(i+1, star = false)
          case _ =>
            multiline(i+1, star = false)
        }
      }
      if (buf(i) == '/') line(i) else multiline(i, star = true)
      out.toString
    }

    /* e.g. `val endOfLine = '\u000A'`*/
    def charlit(j: Int): String = {
      val out = new StringBuilder("'")
      def charlit0(i: Int, bslash: Boolean): Int = {
        if (i == buf.length) i
        else if (i > j+6) { out setLength 0; j }
        else {
          val ch = buf(i)
          out append ch
          ch match {
            case '\\' =>
              charlit0(i+1, bslash = true)
            case '\'' if !bslash =>
              i
            case _ =>
              if (bslash && '0' <= ch && ch <= '9') charlit0(i+1, bslash = true)
              else charlit0(i+1, bslash = false)
          }
        }
      }
      charlit0(j, bslash = false)
      out.toString
    }

    def strlit(i: Int): String = {
      val out = new StringBuilder()

      @tailrec def rawstrlit0(i: Int, bslash: Boolean): Unit = {
        if (i == buf.length) return
        val ch = buf(i)
        out.append(ch)
        ch match {
          case '\\' =>
            rawstrlit0(i+1, bslash = true)
          case '"' if !bslash && buf.slice(i+1, i+3).toString == "\"\"" =>
            out.append("\"\"")
          case _ =>
            rawstrlit0(i+1, bslash = false)
        }
      }

      @tailrec def strlit0(i: Int, bslash: Boolean): Unit = {
        if (i == buf.length) return
        val ch = buf(i)
        out append ch
        ch match {
          case '\\' =>
            strlit0(i+1, bslash = true)
          case '"' if !bslash =>
          case _ =>
            strlit0(i+1, bslash = false)
        }
      }

      buf.slice(i, i+3) match {
        case Array('"','"','"') =>
          out append "\"\"\""
          rawstrlit0(i+3, bslash = false)
        case _ =>
          out append "\""
          strlit0(i+1, bslash = false)
      }
      out.toString
    }

    def numlit(i: Int): String = {
      val out = new StringBuilder
      def intg(i: Int): Int = {
        if (i == buf.length) return i
        val ch = buf(i)
        ch match {
          case '.' =>
            out append ch
            frac(i+1)
          case _ =>
            if (Character.isDigit(ch)) {
              out append ch
              intg(i+1)
            } else i
        }
      }
      def frac(i: Int): Int = {
        if (i == buf.length) return i
        val ch = buf(i)
        ch match {
          case 'e' | 'E' =>
            out append ch
            expo(i+1, signed = false)
          case _ =>
            if (Character.isDigit(ch)) {
              out append ch
              frac(i+1)
            } else i
        }
      }
      def expo(i: Int, signed: Boolean): Int = {
        if (i == buf.length) return i
        val ch = buf(i)
        ch match {
          case '+' | '-' if !signed =>
            out append ch
            expo(i+1, signed = true)
          case _ =>
            if (Character.isDigit(ch)) {
              out append ch
              expo(i+1, signed)
            } else i
        }
      }
      intg(i)
      out.toString
    }

    def escape(str: String): String = {
      val array = str.toCharArray
      val len = array.length
      val buf = new java.lang.StringBuilder(len)
      @tailrec def loop(i: Int): String = {
        if (i < len) {
          (array(i): @switch) match {
            case '<' =>
              buf append "&lt;"
            case '>' =>
              buf append "&gt;"
            case c =>
              buf append c
          }
          loop(i + 1)
        } else {
          buf.toString
        }
      }
      loop(0)
    }

    @tailrec def parse(pre: String, i: Int): Unit = {
      out append pre
      if (i == buf.length) return
      buf(i) match {
        case '\n' =>
          parse("\n", i+1)
        case ' ' =>
          parse(" ", i+1)
        case '&' =>
          parse("&amp;", i+1)
        case '<' if i+1 < buf.length =>
          val ch = buf(i+1)
          if (ch == '-' || ch == ':' || ch == '%')
            parse("<span class=\"kw\">&lt;"+ch+"</span>", i+2)
          else
            parse("&lt;", i+1)
        case '>' =>
          if (i+1 < buf.length && buf(i+1) == ':')
            parse("<span class=\"kw\">&gt;:</span>", i+2)
          else
            parse("&gt;", i+1)
        case '=' =>
          if (i+1 < buf.length && buf(i+1) == '>')
            parse("<span class=\"kw\">=&gt;</span>", i+2)
          else
            parse(buf(i).toString, i+1)
        case '/' =>
          if (i+1 < buf.length && (buf(i+1) == '/' || buf(i+1) == '*')) {
            val c = comment(i+1)
            val escaped = escape(c)
            parse("<span class=\"cmt\">"+escaped+"</span>", i+c.length)
          } else
            parse(buf(i).toString, i+1)
        case '\'' =>
          val s = charlit(i+1)
          if (s.length > 0)
            parse("<span class=\"lit\">"+s+"</span>", i+s.length)
          else
            parse(buf(i).toString, i+1)
        case '"' =>
          val s = strlit(i)
          val escaped = escape(s)
          parse("<span class=\"lit\">"+escaped+"</span>", i+s.length)
        case '@' =>
          val k = lookup(annotations, i+1)
          if (k >= 0)
            parse("<span class=\"ano\">@"+annotations(k)+"</span>", i+annotations(k).length+1)
          else
            parse(buf(i).toString, i+1)
        case _ =>
          if (i == 0 || (i >= 1 && !Character.isJavaIdentifierPart(buf(i-1)))) {
            if (Character.isDigit(buf(i).toInt) ||
                (buf(i) == '.' && i + 1 < buf.length && Character.isDigit(buf(i+1).toInt))) {
              val s = numlit(i)
              parse("<span class=\"num\">"+s+"</span>", i+s.length)
            } else {
              val k = lookup(reserved, i)
              if (k >= 0)
                parse("<span class=\"kw\">"+reserved(k)+"</span>", i+reserved(k).length)
              else {
                val k = lookup(standards, i)
                if (k >= 0)
                  parse("<span class=\"std\">"+standards(k)+"</span>", i+standards(k).length)
                else
                  parse(buf(i).toString, i+1)
              }
            }
          } else
            parse(buf(i).toString, i+1)
      }
    }

    parse("", 0)
    Raw(out.toString) :: NoElems
  }
}
