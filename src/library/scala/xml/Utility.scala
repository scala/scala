/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

import scala.collection.mutable
import parsing.XhtmlEntities

/**
 * The `Utility` object provides utility functions for processing instances
 * of bound and not bound XML classes, as well as escaping text nodes.
 *
 * @author Burak Emir
 */
object Utility extends AnyRef with parsing.TokenTests {
  final val SU = '\u001A'

  implicit def implicitSbToString(sb: StringBuilder) = sb.toString()

  // helper for the extremely oft-repeated sequence of creating a
  // StringBuilder, passing it around, and then grabbing its String.
  private [xml] def sbToString(f: (StringBuilder) => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }
  private[xml] def isAtomAndNotText(x: Node) = x.isAtom && !x.isInstanceOf[Text]

  /** Trims an element - call this method, when you know that it is an
   *  element (and not a text node) so you know that it will not be trimmed
   *  away. With this assumption, the function can return a `Node`, rather
   *  than a `Seq[Node]`. If you don't know, call `trimProper` and account
   *  for the fact that you may get back an empty sequence of nodes.
   *
   *  Precondition: node is not a text node (it might be trimmed)
   */
  def trim(x: Node): Node = x match {
    case Elem(pre, lab, md, scp, child@_*) =>
      Elem(pre, lab, md, scp, (child flatMap trimProper):_*)
  }

  /** trim a child of an element. `Attribute` values and `Atom` nodes that
   *  are not `Text` nodes are unaffected.
   */
  def trimProper(x:Node): Seq[Node] = x match {
    case Elem(pre,lab,md,scp,child@_*) =>
      Elem(pre,lab,md,scp, (child flatMap trimProper):_*)
    case Text(s) =>
      new TextBuffer().append(s).toText
    case _ =>
      x
  }

  /** returns a sorted attribute list */
  def sort(md: MetaData): MetaData = if((md eq Null) || (md.next eq Null)) md else {
    val key = md.key
    val smaller = sort(md.filter { m => m.key < key })
    val greater = sort(md.filter { m => m.key > key })
    smaller.copy(md.copy ( greater ))
  }

  /** Return the node with its attribute list sorted alphabetically
   *  (prefixes are ignored) */
  def sort(n:Node): Node = n match {
	case Elem(pre,lab,md,scp,child@_*) =>
      Elem(pre,lab,sort(md),scp, (child map sort):_*)
    case _ => n
  }

  /**
   * Escapes the characters &lt; &gt; &amp; and &quot; from string.
   *
   * @param text ...
   * @return     ...
   */
  final def escape(text: String): String = sbToString(escape(text, _))

  object Escapes {
    /** For reasons unclear escape and unescape are a long ways from
      * being logical inverses. */
    val pairs = Map(
      "lt"    -> '<',
      "gt"    -> '>',
      "amp"   -> '&',
      "quot"  -> '"'
      // enigmatic comment explaining why this isn't escaped --
      // is valid xhtml but not html, and IE doesn't know it, says jweb
      // "apos"  -> '\''
    )
    val escMap    = pairs map { case (s, c) => c-> ("&%s;" format s) }
    val unescMap  = pairs ++ Map("apos"  -> '\'')
  }
  import Escapes.{ escMap, unescMap }

  /**
   * Appends escaped string to `s`.
   *
   * @param text ...
   * @param s    ...
   * @return     ...
   */
  final def escape(text: String, s: StringBuilder): StringBuilder = {
    // Implemented per XML spec:
    // http://www.w3.org/International/questions/qa-controls
    // imperative code 3x-4x faster than current implementation
    // dpp (David Pollak) 2010/02/03
    val len = text.length
    var pos = 0
    while (pos < len) {
      text.charAt(pos) match {
        case '<' => s.append("&lt;")
        case '>' => s.append("&gt;")
        case '&' => s.append("&amp;")
        case '"' => s.append("&quot;")
        case '\n' => s.append('\n')
        case '\r' => s.append('\r')
        case '\t' => s.append('\t')
        case c => if (c >= ' ') s.append(c)
      }

      pos += 1
    }
    s
  }

  /**
   * Appends unescaped string to `s`, `amp` becomes `&amp;`,
   * `lt` becomes `&lt;` etc..
   *
   * @param ref ...
   * @param s   ...
   * @return    `'''null'''` if `ref` was not a predefined entity.
   */
  final def unescape(ref: String, s: StringBuilder): StringBuilder =
    (unescMap get ref) map (s append _) orNull

  /**
   * Returns a set of all namespaces used in a sequence of nodes
   * and all their descendants, including the empty namespaces.
   *
   * @param nodes ...
   * @return      ...
   */
  def collectNamespaces(nodes: Seq[Node]): mutable.Set[String] =
    nodes.foldLeft(new mutable.HashSet[String]) { (set, x) => collectNamespaces(x, set) ; set }

  /**
   * Adds all namespaces in node to set.
   *
   * @param n   ...
   * @param set ...
   */
  def collectNamespaces(n: Node, set: mutable.Set[String]) {
    if (n.doCollectNamespaces) {
      set += n.namespace
      for (a <- n.attributes) a match {
        case _:PrefixedAttribute =>
          set += a.getNamespace(n)
        case _ =>
      }
      for (i <- n.child)
        collectNamespaces(i, set)
    }
  }

  // def toXML(
  //   x: Node,
  //   pscope: NamespaceBinding = TopScope,
  //   sb: StringBuilder = new StringBuilder,
  //   stripComments: Boolean = false,
  //   decodeEntities: Boolean = true,
  //   preserveWhitespace: Boolean = false,
  //   minimizeTags: Boolean = false): String =
  // {
  //   toXMLsb(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
  //   sb.toString()
  // }

  def toXML(
    x: Node,
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = false): StringBuilder =
  {
    x match {
      case c: Comment => if (!stripComments) c buildString sb else sb
      case x: SpecialNode => x buildString sb
      case g: Group =>
        g.nodes foreach {toXML(_, x.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)}
        sb
      case _  =>
        // print tag with namespace declarations
        sb.append('<')
        x.nameToString(sb)
        if (x.attributes ne null) x.attributes.buildString(sb)
        x.scope.buildString(sb, pscope)
        if (x.child.isEmpty && minimizeTags) {
          // no children, so use short form: <xyz .../>
          sb.append(" />")
        } else {
          // children, so use long form: <xyz ...>...</xyz>
          sb.append('>')
          sequenceToXML(x.child, x.scope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
          sb.append("</")
          x.nameToString(sb)
          sb.append('>')
        }
    }
  }

  def sequenceToXML(
    children: Seq[Node],
    pscope: NamespaceBinding = TopScope,
    sb: StringBuilder = new StringBuilder,
    stripComments: Boolean = false,
    decodeEntities: Boolean = true,
    preserveWhitespace: Boolean = false,
    minimizeTags: Boolean = false): Unit =
  {
    if (children.isEmpty) return
    else if (children forall isAtomAndNotText) { // add space
      val it = children.iterator
      val f = it.next
      toXML(f, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      while (it.hasNext) {
        val x = it.next
        sb.append(' ')
        toXML(x, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags)
      }
    }
    else children foreach { toXML(_, pscope, sb, stripComments, decodeEntities, preserveWhitespace, minimizeTags) }
  }

  /**
   * Returns prefix of qualified name if any.
   *
   * @param name ...
   * @return     ...
   */
  final def prefix(name: String): Option[String] = (name indexOf ':') match {
    case -1   => None
    case i    => Some(name.substring(0, i))
  }

  /**
   * Returns a hashcode for the given constituents of a node
   *
   * @param uri
   * @param label
   * @param attribHashCode
   * @param children
   */
  def hashCode(pre: String, label: String, attribHashCode: Int, scpeHash: Int, children: Seq[Node]) =
    scala.util.MurmurHash3.orderedHash(label +: attribHashCode +: scpeHash +: children, pre.##)

  def appendQuoted(s: String): String = sbToString(appendQuoted(s, _))

  /**
   * Appends &quot;s&quot; if string `s` does not contain &quot;,
   * &apos;s&apos; otherwise.
   *
   * @param s  ...
   * @param sb ...
   * @return   ...
   */
  def appendQuoted(s: String, sb: StringBuilder) = {
    val ch = if (s contains '"') '\'' else '"'
    sb.append(ch).append(s).append(ch)
  }

  /**
   * Appends &quot;s&quot; and escapes and &quot; i s with \&quot;
   *
   * @param s  ...
   * @param sb ...
   * @return   ...
   */
  def appendEscapedQuoted(s: String, sb: StringBuilder): StringBuilder = {
    sb.append('"')
    for (c <- s) c match {
      case '"' => sb.append('\\'); sb.append('"')
      case _   => sb.append(c)
    }
    sb.append('"')
  }

  /**
   * @param s     ...
   * @param index ...
   * @return      ...
   */
  def getName(s: String, index: Int): String = {
    if (index >= s.length) null
    else {
      val xs = s drop index
      if (xs.nonEmpty && isNameStart(xs.head)) xs takeWhile isNameChar
      else ""
    }
  }

  /**
   * Returns `'''null'''` if the value is a correct attribute value,
   * error message if it isn't.
   *
   * @param value ...
   * @return      ...
   */
  def checkAttributeValue(value: String): String = {
    var i = 0
    while (i < value.length) {
      value.charAt(i) match {
        case '<' =>
          return "< not allowed in attribute value";
        case '&' =>
          val n = getName(value, i+1)
          if (n eq null)
            return "malformed entity reference in attribute value ["+value+"]";
          i = i + n.length + 1
          if (i >= value.length || value.charAt(i) != ';')
            return "malformed entity reference in attribute value ["+value+"]";
        case _   =>
      }
      i = i + 1
    }
    null
  }

  /**
   * new
   *
   * @param value ...
   * @return      ...
   */
  def parseAttributeValue(value: String): Seq[Node] = {
    val sb  = new StringBuilder
    var rfb: StringBuilder = null
    val nb = new NodeBuffer()

    val it = value.iterator
    while (it.hasNext) {
      var c = it.next
      // entity! flush buffer into text node
      if (c == '&') {
        c = it.next
        if (c == '#') {
          c = it.next
          val theChar = parseCharRef ({ ()=> c },{ () => c = it.next },{s => throw new RuntimeException(s)}, {s => throw new RuntimeException(s)})
          sb.append(theChar)
        }
        else {
          if (rfb eq null) rfb = new StringBuilder()
          rfb append c
          c = it.next
          while (c != ';') {
            rfb.append(c)
            c = it.next
          }
          val ref = rfb.toString()
          rfb.clear()
          unescape(ref,sb) match {
            case null =>
              if (sb.length > 0) {  // flush buffer
                nb += Text(sb.toString())
                sb.clear()
              }
              nb += EntityRef(ref) // add entityref
            case _ =>
          }
        }
      }
      else sb append c
    }
    if (sb.length > 0) { // flush buffer
      val x = Text(sb.toString())
      if (nb.length == 0)
        return x
      else
        nb += x
    }
    nb
  }

  /**
   * {{{
   *   CharRef ::= "&amp;#" '0'..'9' {'0'..'9'} ";"
   *             | "&amp;#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   * }}}
   * See [66]
   *
   * @param ch                ...
   * @param nextch            ...
   * @param reportSyntaxError ...
   * @return                  ...
   */
  def parseCharRef(ch: () => Char, nextch: () => Unit, reportSyntaxError: String => Unit, reportTruncatedError: String => Unit): String = {
    val hex  = (ch() == 'x') && { nextch(); true }
    val base = if (hex) 16 else 10
    var i = 0
    while (ch() != ';') {
      ch() match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + ch().asDigit
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if (! hex)
            reportSyntaxError("hex char not allowed in decimal char ref\n" +
                              "Did you mean to write &#x ?")
          else
            i = i * base + ch().asDigit
        case SU =>
          reportTruncatedError("")
        case _ =>
          reportSyntaxError("character '" + ch() + "' not allowed in char ref\n")
      }
      nextch()
    }
    new String(Array(i), 0, 1)
  }
}
