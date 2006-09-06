/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import scala.runtime.compat.StringBuilder
import scala.collection.mutable

/**
 * Utility functions for processing instances of bound and
 * not bound XML classes, as well as escaping text nodes.
 */
object Utility extends AnyRef with parsing.TokenTests {

  def view(s: String): Text = Text(s)

  /* escapes the characters &lt; &gt; &amp; and &quot; from string */
  final def escape(text: String): String =
    escape(text, new StringBuilder()).toString()

  /* appends escaped string to s */
  final def escape(text: String, s: StringBuilder): StringBuilder = {
    for (val c <- Iterator.fromString(text)) c match {
      case '<' => s.append("&lt;")
      case '>' => s.append("&gt;")
      case '&' => s.append("&amp;")
      case '"' => s.append("&quot;")
      case _   => s.append(c)
    }
    s
  }

  /**
   * Returns a set of all namespaces used in a sequence of nodes
   * and all their descendants, including the empty namespaces.
   *
   * @param nodes
   */

  def collectNamespaces(nodes: Seq[Node]): mutable.Set[String] = {
    var m = new mutable.HashSet[String]()
    val it = nodes.elements
    while (it.hasNext)
      collectNamespaces(it.next, m);
    m
  }

  /** adds all namespaces in node to set */
  def collectNamespaces(n: Node, set: mutable.Set[String]): Unit = {
    if( n.typeTag$ >= 0 ) {
      set += n.namespace;
      for (val a <- n.attributes) a match {
          case _:PrefixedAttribute =>
            set += a.getNamespace(n)
          case _ =>
        }
      for (val i <- n.child)
        collectNamespaces(i, set);
    }
  }

  /** string representation of an XML node, with comments stripped the comments
   * @see "toXML(Node, Boolean)"
   */
  def toXML(n: Node): String = toXML(n, true);

  /**
   * String representation of a Node. uses namespace mapping from
   * <code>defaultPrefixes(n)</code>.
   *
   * @param n
   * @param stripComment
   *
   * @todo define a way to escape literal characters to &amp;xx; references
   */
  def toXML(n: Node, stripComment: Boolean): String = {
    val sb = new StringBuilder()
    toXML(n, TopScope, sb, stripComment)
    sb.toString()
  }


  /** appends a tree to the given stringbuffer within given namespace scope.
   *
   *   @param n            the node
   *   @param pscope       the parent scope
   *   @param sb           stringbuffer to append to
   *   @param stripComment if true, strip comments
   */
  def toXML(x: Node, pscope: NamespaceBinding, sb: StringBuilder, stripComment: Boolean): Unit = {
    x match {

      case c: Comment if !stripComment =>
        c.toString(sb)

      case x: SpecialNode =>
        x.toString(sb)

      case g: Group =>
        for (val c <- g.nodes) {
          toXML(c, x.scope, sb, stripComment)
        }

      case _  =>
        // print tag with namespace declarations
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes != null) {
          x.attributes.toString(sb)
        }
        x.scope.toString(sb, pscope)
        sb.append('>')
        sequenceToXML(x.child, pscope, sb, stripComment)
        sb.append("</")
        x.nameToString(sb)
        sb.append('>')
    }
  }

  def sequenceToXML(children: Seq[Node], pscope: NamespaceBinding, sb: StringBuilder, stripComment: Boolean): Unit = {
    if(children.isEmpty)
      return
    else if(children exists { y => y.isInstanceOf[Atom[Any]] }) {
      val it = children.elements
      val f = it.next
      toXML(f, f.scope, sb, stripComment)
      while(it.hasNext) {
        val x = it.next
        sb.append(' ')
        toXML(x, x.scope, sb, stripComment)
      }
    } else for (val c <- children) {
      toXML(c, c.scope, sb, stripComment)
    }
  }


  /** returns prefix of qualified name if any */
  final def prefix(name: String): Option[String] = {
    val i = name.indexOf(':'.asInstanceOf[Int])
    if( i != -1 ) Some( name.substring(0, i) ) else None
  }

  /**
   * Returns a hashcode for the given constituents of a node
   *
   * @param uri
   * @param label
   * @param attribHashCode
   * @param children
   */
  def hashCode(pre: String, label: String, attribHashCode: Int, scpeHash: Int, children: Seq[Node]) = {
    ( if(pre!=null) {41 * pre.hashCode() % 7} else {0})
    + label.hashCode() * 53
    + attribHashCode * 7
    + scpeHash * 31
    + children.hashCode()
  }

  /**
   * Returns a hashcode for the given constituents of a node
   *
   * @param uri
   * @param label
   * @param attribs
   * @param children
  def hashCode(uri: String, label: String, attribs: scala.collection.mutable.HashMap[Pair[String,String],String], scpe: Int, children: Seq[Node]): Int = {
    41 * uri.hashCode() % 7 + label.hashCode() + attribs.toList.hashCode() + scpe + children.hashCode()
  }
   */

  def systemLiteralToString(s: String): String = {
    val sb = new StringBuilder()
    systemLiteralToString(sb, s)
    sb.toString()
  }

  def systemLiteralToString(sb: StringBuilder, s: String): StringBuilder = {
    sb.append("SYSTEM ")
    appendQuoted(s, sb)
  }

  def publicLiteralToString(s: String): String = {
    val sb = new StringBuilder()
    systemLiteralToString(sb, s)
    sb.toString()
  }

  def publicLiteralToString(sb: StringBuilder, s: String): StringBuilder = {
    sb.append("PUBLIC \"").append(s).append('"')
  }

  /**
   * Appends &quot;s&quot; if s does not contain &quot;, &apos;s&apos;
   * otherwise
   *
   * @param s
   * @param sb
   */
  def appendQuoted(s: String, sb: StringBuilder) = {
    val ch = if (s.indexOf('"'.asInstanceOf[Int]) == -1) '"' else '\'';
    sb.append(ch).append(s).append(ch)
  }

  /**
   * Appends &quot;s&quot; and escapes and &quot; i s with \&quot;
   *
   * @param s
   * @param sb
   */
  def appendEscapedQuoted(s: String, sb: StringBuilder) = {
    sb.append('"')
    val z:Seq[Char] = Predef.string2seq(s)
    for( val c <- z ) c match {
      case '"' => sb.append('\\'); sb.append('"')
      case _   => sb.append(c)
    }
    sb.append('"')
  }

  def getName(s: String, index: Int): String = {
    var i = index;
    val sb = new StringBuilder();
    if (i < s.length()) {
      var c = s.charAt(i);
      if (isNameStart(s.charAt(i)))
        while (i < s.length() && { c = s.charAt(i); isNameChar(c)}) {
          sb.append(c);
          i = i + 1
        }
      sb.toString()
    } else null
  }

  /** returns null if the value is a correct attribute value, error message if it isn't */
  def checkAttributeValue(value: String): String = {
    var i = 0
    while (i < value.length()) {
      value.charAt(i) match {
        case '<' =>
          return "< not allowed in attribute value";
        case '&' =>
          val n = getName(value, i+1);
          if (n== null)
            return "malformed entity reference in attribute value ["+value+"]";
          i = i + n.length() + 1;
          if (i >= value.length() || value.charAt(i) != ';')
            return "malformed entity reference in attribute value ["+value+"]";
        case _   =>
      }
      i = i + 1
    }
    null
  }

  /** new
   */
  def parseAttributeValue(value:String):Seq[Node] = {
    val zs:Seq[Char] = value
    val sb = new StringBuilder()
    val nb = new NodeBuffer()
    val it = zs.elements
    while(it.hasNext) {
      var c = it.next
      c match {
        case '&' =>
          if(sb.length() > 0) {
            nb += Text(sb.toString())
            sb.setLength(0)
          }
          it.next match {
            case '#' =>
              c = it.next
              val theChar = parseCharRef ({ ()=> c },{ () => c = it.next },{s => throw new RuntimeException(s)})
              sb.append(theChar)

            case x =>
              sb.append(x)
              c = it.next
              while(c != ';') {
                sb.append(c)
                c = it.next
              }
              nb += EntityRef(sb.toString())
              sb.setLength(0)
          }
        case x   =>
          sb.append(x)
      }
    }
    if(sb.length() > 0) {
      val x = Text(sb.toString())
      if(nb.length == 0)
        return x
      else
        nb += x
    }
    return nb
  }

  /** CharRef ::= "&amp;#" '0'..'9' {'0'..'9'} ";"
   *            | "&amp;#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   *
   * see [66]
   */
  def parseCharRef(ch: () => Char, nextch: () => Unit, reportSyntaxError:(String) => Unit): String = {
    val hex  = (ch() == 'x') && { nextch(); true };
    val base = if (hex) 16 else 10;
    var i = 0;
    while (ch() != ';') {
      ch() match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + Character.digit( ch(), base );
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if (! hex)
            reportSyntaxError("hex char not allowed in decimal char ref\n"
                         +"Did you mean to write &#x ?");
          else
            i = i * base + Character.digit(ch(), base);
        case _ =>
          reportSyntaxError("character '" + ch() + " not allowed in char ref\n");
      }
      nextch();
    }
    String.valueOf(i.asInstanceOf[char])
  }

}
