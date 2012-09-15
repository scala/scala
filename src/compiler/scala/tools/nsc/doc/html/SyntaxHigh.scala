/* NSC -- new Scala compiler
 * Copyright 2010-2012 LAMP/EPFL
 * @author  Stephane Micheloud
 */

package scala.tools.nsc.doc.html

import scala.xml.NodeSeq

/** Highlight the syntax of Scala code appearing in a `{{{` wiki block
  * (see method `HtmlPage.blockToHtml`).
  *
  * @author Stephane Micheloud
  * @version 1.0
  */
private[html] object SyntaxHigh {

  /** Reserved words, sorted alphabetically
    * (see [[scala.reflect.internal.StdNames]]) */
  val reserved = Array(
    "abstract", "case", "catch", "class", "def",
    "do", "else", "extends", "false", "final", "finally",
    "for", "if", "implicit", "import", "lazy", "match",
    "new", "null", "object", "override", "package",
    "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "true", "try", "type",
    "val", "var", "while", "with", "yield")

  /** Annotations, sorted alphabetically */
  val annotations = Array(
    "BeanProperty", "SerialVersionUID",
    "beanGetter", "beanSetter", "bridge", "cloneable",
    "deprecated", "deprecatedName",
    "elidable", "field", "getter", "inline",
    "migration", "native", "noinline", "param",
    "remote", "setter", "specialized", "strictfp", "switch",
    "tailrec", "throws", "transient",
    "unchecked", "uncheckedStable", "uncheckedVariance",
    "varargs", "volatile")

  /** Standard library classes/objects, sorted alphabetically */
  val standards = Array (
    "WeakTypeTag", "Any", "AnyRef", "AnyVal", "App", "Application", "Array",
    "Boolean", "Byte", "Char", "Class", "ClassTag", "ClassManifest",
    "Console", "Double", "Enumeration", "Float", "Function", "Int",
    "List", "Long", "Manifest", "Map",
    "NoManifest", "None", "Nothing", "Null", "Object", "Option", "OptManifest",
    "Pair", "Predef",
    "Seq", "Set", "Short", "Some", "String", "Symbol",
    "Triple", "TypeTag", "Unit")

  def apply(data: String): NodeSeq = {
    val buf = data.getBytes
    val out = new StringBuilder

    def compare(offset: Int, key: String): Int = {
      var i = offset
      var j = 0
      val l = key.length
      while (i < buf.length && j < l) {
        val bch = buf(i).toChar
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
          out append buf(i).toChar
          line(i+1)
        }
      var level = 0
      def multiline(i: Int, star: Boolean): Int = {
        if (i == buf.length) return i
        val ch = buf(i).toChar
        out append ch
        ch match {
          case '*' =>
            if (star) level += 1
            multiline(i+1, !star)
          case '/' =>
            if (star) {
              if (level > 0) level -= 1
              if (level == 0) i else multiline(i+1, true)
            } else
              multiline(i+1, false)
          case _ =>
            multiline(i+1, false)
        }
      }
      if (buf(i) == '/') line(i) else multiline(i, true)
      out.toString
    }

    /* e.g. `val endOfLine = '\u000A'`*/
    def charlit(j: Int): String = {
      val out = new StringBuilder("'")
      def charlit0(i: Int, bslash: Boolean): Int = {
        if (i == buf.length) i
        else if (i > j+6) { out setLength 0; j }
        else {
          val ch = buf(i).toChar
          out append ch
          ch match {
            case '\\' =>
              charlit0(i+1, true)
            case '\'' if !bslash =>
              i
            case _ =>
              if (bslash && '0' <= ch && ch <= '9') charlit0(i+1, true)
              else charlit0(i+1, false)
          }
        }
      }
      charlit0(j, false)
      out.toString
    }

    def strlit(i: Int): String = {
      val out = new StringBuilder("\"")
      def strlit0(i: Int, bslash: Boolean): Int = {
        if (i == buf.length) return i
        val ch = buf(i).toChar
        out append ch
        ch match {
          case '\\' =>
            strlit0(i+1, true)
          case '"' if !bslash =>
            i
          case _ =>
            strlit0(i+1, false)
        }
      }
      strlit0(i, false)
      out.toString
    }

    def numlit(i: Int): String = {
      val out = new StringBuilder
      def intg(i: Int): Int = {
        if (i == buf.length) return i
        val ch = buf(i).toChar
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
        val ch = buf(i).toChar
        ch match {
          case 'e' | 'E' =>
            out append ch
            expo(i+1, false)
          case _ =>
            if (Character.isDigit(ch)) {
              out append ch
              frac(i+1)
            } else i
        }
      }
      def expo(i: Int, signed: Boolean): Int = {
        if (i == buf.length) return i
        val ch = buf(i).toChar
        ch match {
          case '+' | '-' if !signed =>
            out append ch
            expo(i+1, true)
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

    def parse(pre: String, i: Int): Int = {
      out append pre
      if (i == buf.length) return i
      buf(i) match {
        case '\n' =>
          parse("\n", i+1)
        case ' ' =>
          parse(" ", i+1)
        case '&' =>
          parse("&amp;", i+1)
        case '<' if i+1 < buf.length =>
          val ch = buf(i+1).toChar
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
            parse(buf(i).toChar.toString, i+1)
        case '/' =>
          if (i+1 < buf.length && (buf(i+1) == '/' || buf(i+1) == '*')) {
            val c = comment(i+1)
            parse("<span class=\"cmt\">"+c+"</span>", i+c.length)
          } else
            parse(buf(i).toChar.toString, i+1)
        case '\'' =>
          val s = charlit(i+1)
          if (s.length > 0)
            parse("<span class=\"lit\">"+s+"</span>", i+s.length)
          else
            parse(buf(i).toChar.toString, i+1)
        case '"' =>
          val s = strlit(i+1)
          parse("<span class=\"lit\">"+s+"</span>", i+s.length)
        case '@' =>
          val k = lookup(annotations, i+1)
          if (k >= 0)
            parse("<span class=\"ano\">@"+annotations(k)+"</span>", i+annotations(k).length+1)
          else
            parse(buf(i).toChar.toString, i+1)
        case _ =>
          if (i == 0 || (i >= 1 && !Character.isJavaIdentifierPart(buf(i-1).toChar))) {
            if (Character.isDigit(buf(i)) ||
                (buf(i) == '.' && i + 1 < buf.length && Character.isDigit(buf(i+1)))) {
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
                  parse(buf(i).toChar.toString, i+1)
              }
            }
          } else
            parse(buf(i).toChar.toString, i+1)
      }
      i
    }

    parse("", 0)
    scala.xml.Unparsed(out.toString)
  }
}
