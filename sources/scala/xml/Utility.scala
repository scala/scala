/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml;

import java.lang.StringBuffer; /* Java dependency! */
import scala.collection.mutable;
import scala.collection.immutable;
import scala.collection.Map;

/**
 * Utility functions for processing instances of bound and not bound XML
 * classes, as well as escaping text nodes
 */
object Utility {

  def view(s: String): Text = Text(s);

  /* escapes the characters &lt; &gt; &amp; and &quot; from string */
  def escape(text: String) = {
    val s = new StringBuffer();
    for (val c <- Iterator.fromString(text)) c match {
      case '<' => s.append("&lt;");
      case '>' => s.append("&gt;");
      case '&' => s.append("&amp;");
      case '"' => s.append("&quot;");
      case _   => s.append(c);
    }
    s.toString()
  }

  /**
   * Returns a set of all namespaces appearing in a node and all its
   * descendants, including the empty namespaces
   *
   * @param node
   */
  def collectNamespaces(node: Node): mutable.Set[String] = {
    collectNamespaces(node, new mutable.HashSet[String]());
  }

  /**
   * Returns a set of all namespaces appearing in a sequence of nodes
   * and all their descendants, including the empty namespaces
   *
   * @param nodes
   */
  def collectNamespaces(nodes: Seq[Node]): mutable.Set[String] = {
    var m = new mutable.HashSet[String]();
    for (val n <- nodes)
      collectNamespaces(n, m);
    m
  }

  private def collectNamespaces(node: Node, set: mutable.Set[String]): mutable.Set[String] = {
    def collect( n:Node ):Unit = {
      if( n.typeTag$ >= 0 ) {
        set += n.namespace; /* namespaces are interned, so hashing is fast */
        for (val a <- n.attributes)
          if (a.namespace.length() > 0)  // == 0 should not happen, but safer.
            set += a.namespace;
        for (val i <- n.child)
          collect(i);
      }
    }
    collect(node);
    set
  }

  /**
   * A prefix mapping that maps the empty namespace to the empty prefix
   */
  val noPrefixes: Map[String,String] =
    immutable.ListMap.Empty[String,String].update("","");

  /**
   * Returns a default prefix mapping for a set of namespaces.
   * the empty namespace is mapped to the empty prefix
   *
   * @param rootns
   * @param nset
   */
  def defaultPrefixes(rootns: String, nset: mutable.Set[String]): Map[String,String] = {
    val map = new mutable.HashMap[String,String]();
    if (nset.contains("http://www.w3.org/XML/1998/namespace"))
       map.update("http://www.w3.org/XML/1998/namespace", "xml");
    if (nset.contains(""))
      map.update("", "");
    var i = 0;
    for (val ns <- nset)
      map.get(ns) match {
        case None      => map.update( ns, "ns"+i ); i = i + 1;
        case Some( _ ) =>
      }
    // Console.println("defaultPrefixes:"+map); // DEBUG
    return map;
  }

  /**
   * @see "defaultPrefixes(String, mutable.Set[String])"
   */
  def defaultPrefixes(n: Node): Map[String,String] =
    defaultPrefixes(n.namespace, collectNamespaces( n ));

  /**
   * @see "defaultPrefixes(String, mutable.Set[String])"
   */
  def defaultPrefixes(nodes: Seq[Node]): Map[String,String] =
    defaultPrefixes("", collectNamespaces(nodes));



  /**
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
    val s = new StringBuffer();
    toXML(n, defaultPrefixes(n), s, stripComment);
    s.toString();
  }

  /**
   * @see "toXML(Node, Map[String,String], Boolean)"
   */
  def toXML(x: Node, pmap: Map[String,String]): String =
    toXML(x, pmap, false);

  /**
   * Serializes a node with given namespace prefix mapping. The prefix
   * mapping may not map the empty namespace "" to some non-empty prefix.
   *
   * @param x the node to serialize
   * @param pmap a mapping from namespace URIs to prefixes
   * @param stripComment
   */
  def toXML(x: Node, pmap: Map[String,String], stripComment: Boolean): String = {
    val sb = new StringBuffer();
    toXML(x, pmap, sb, stripComment);
    sb.toString();
  }

  /**
   * @see "toXML(Node, Map[String,String], StringBuffer, Boolean)"
   */
  def toXML(x: Node, pmap: Map[String,String], sb: StringBuffer): Unit =
    toXML(x, pmap, sb, false);

  /** serializes a tree to the given stringbuffer
   *  with the given namespace prefix mapping.
   *  elements and attributes that have namespaces not in pmap are <strong>ignored</strong>
   *   @param n            the root node
   *   @param pmap         mapping namespaces to prefixes
   *   @param sb           stringbuffer to append to
   *   @param stripComment if true, strip comments
   */
  def toXML(x: Node, pmap: Map[String,String], sb: StringBuffer, stripComment: Boolean): Unit = {
    x match {

      case Text(t) =>
        sb.append(escape(t));

      case Comment(text) =>
        if (stripComment) text else "";

      case _ if x.typeTag$ < 0 =>
        sb.append( x.toString() );

      case _ if( pmap.contains( x.namespace )) => {
        // print tag with namespace declarations
        sb.append('<');
        appendPrefixedName( x.namespace, x.label, pmap, sb );
        if (x.attributes.length != 0) {
          attr2xml( x.namespace, x.attributes.elements, pmap, sb )
        }
        if ((pmap.size != 1) || pmap.get("").isEmpty) {
          for (val Pair(ns, pref) <- pmap.elements; pref!="xml") {
            sb.append(' ');
            sb.append("xmlns");
            if(pref.length() > 0) sb.append(':');
            sb.append(pref);
            sb.append("=\"");
            sb.append(ns);
            sb.append('"')
          }
        }
        sb.append('>');
        for (val c <- x.child.elements) {
          toXML1(c, pmap, sb, stripComment);
        }
        sb.append("</");
        appendPrefixedName(x.namespace, x.label, pmap, sb);
        sb.append('>')
      }
    }
  }

  /**
   * Serializes a non-root node to the given stringbuffer
   * with the given namespace prefix mapping. use toXML to print the
   * root node, which will have namespace declarations.
   *
   * @param n the non-root node
   * @param pmap         mapping namespaces to prefixes
   * @param sb           stringbuffer to append to
   * @param stripComment if true, strip comments
   */
  def toXML1(x: Node, pmap: Map[String,String], sb: StringBuffer, stripComment: Boolean): Unit = {
    //Console.print("toString: "+x.toString());
    x match {
      case Text(t) =>
        sb.append(escape(t));

      case Comment(text) =>
        if (stripComment) text else "";

      case _ if x.typeTag$ < 0 =>
        sb.append( x.toString() );

      case _ if( pmap.contains( x.namespace )) => {
        sb.append('<');
        appendPrefixedName(x.namespace, x.label, pmap, sb);
        if (x.attributes.length != 0) {
          attr2xml(x.namespace, x.attributes.elements, pmap, sb)
        }
        sb.append('>');
        for (val c <- x.child.elements) {
          toXML1(c, pmap, sb, stripComment);
        }
        sb.append("</");
        appendPrefixedName(x.namespace, x.label, pmap, sb);
        sb.append('>');
      }
    }
  }

  /**
   * @see "toXML1(Node, Map[String,String], StringBuffer, Boolean)"
   */
  def toXML1(x: Node, pmap: Map[String,String], sb: StringBuffer): Unit =
    toXML1(x, pmap, sb, false);

  /**
   * For a Node n, returns string representation of <code>n.attributes</code>
   *
   * @param ns
   * @param attrib
   * @param pmap
   * @param sb
   */
  def attr2xml(ns: String, attrib: Iterator[Attribute], pmap: Map[String, String], sb: StringBuffer ) = {
    for (val x <- attrib) {
      sb.append(' ');
      if (ns == x.namespace)
        sb.append(x.key);
      else
        appendPrefixedName(x.namespace, x.key, pmap, sb);
      sb.append("=");
      appendQuoted(x.value, sb)
    }
  }

  /**
   * Returns a hashcode for the given constituents of a node
   *
   * @param uname
   * @param attribHashCode
   * @param children
   */
  def hashCode(uname: UName, attribHashCode: Int, children: Seq[Node]): Int = {
    hashCode(uname.uri, uname.label, attribHashCode, children);
  }

  /**
   * Returns a hashcode for the given constituents of a node
   *
   * @param uri
   * @param label
   * @param attribHashCode
   * @param children
   */
  def hashCode(uri: String, label: String, attribHashCode: Int, children: Seq[Node]) = {
    41 * uri.hashCode() % 7 + label.hashCode() + attribHashCode + children.hashCode()
  }

  /**
   * Returns a hashcode for the given constituents of a node
   *
   * @param uri
   * @param label
   * @param attribs
   * @param children
   */
  def hashCode(uri: String, label: String, attribs: scala.collection.mutable.HashMap[Pair[String,String],String], children: Seq[Node]): Int = {
    41 * uri.hashCode() % 7 + label.hashCode() + attribs.toList.hashCode() + children.hashCode()
  }

  def systemLiteralToString(s: String) = {
    val sb = new StringBuffer("SYSTEM ");
    appendQuoted(s, sb);
    sb.toString();
  }

  def publicLiteralToString(s: String) = {
    val sb = new StringBuffer("PUBLIC ");
    sb.append('"').append(s).append('"');
    sb.toString();
  }

  /**
   * Appends prefixed name to given stringbuffer using
   * namespace-to-prefix mapping pmap.
   *
   * precondition: pmap.contains(ns)
   *
   * @param ns
   * @param name
   * @param pmap
   * @param sb
   */
  def appendPrefixedName(ns: String, name: String, pmap: Map[String, String], sb: StringBuffer): Unit = {
    val pref = pmap( ns );
    if (pref.length() > 0) {
      sb.append(pref);
      sb.append(':')
    }
    sb.append(name)
  }

  /**
   * Appends &quot;s&quot; if s does not contain &quot;, &apos;s&apos;
   * otherwise
   *
   * @param s
   * @param sb
   */
  def appendQuoted(s: String, sb: StringBuffer) = {
    val ch = if (s.indexOf('"') == -1) '"' else '\'';
    sb.append(ch).append(s).append(ch)
  }

  /**
   * Appends &quot;s&quot; and escapes and &quot; i s with \&quot;
   *
   * @param s
   * @param sb
   */
  def appendEscapedQuoted(s: String, sb: StringBuffer) = {
    sb.append('"');
    val z:Seq[Char] = s;
    for( val c <- z ) c match {
      case '"' => sb.append('\\'); sb.append('"');
      case _   => sb.append( c );
    }
    sb.append('"')
  }


}
