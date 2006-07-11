/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;

import scala.runtime.compat.StringBuilder

/** The case class <code>Elem</code> extends the Node class,
 *  providing an immutable data object representing an XML element.
 *
 *  @param prefix (may be null)
 *  @param label the element name
 *  @param attribute the attribute map
 *  @param child the children of this node
 *  @author  Burak Emir
 */
// "val" is redundant for non-overriding arguments
case class Elem(override val prefix: String,
                val label: String,
                override val attributes: MetaData,
                override val scope: NamespaceBinding,
                val child: Node*) extends Node {

  if (prefix != null && 0 == prefix.length())
    scala.Predef.error("prefix of zero length, use null instead");

  if (null == scope)
    scala.Predef.error("scope is null");

  //@todo: copy the children,
  //  setting namespace scope if necessary
  //  cleaning adjacent text nodes if necessary

  //final val namespaceIntern     = namespace$$.intern();
  //final def namespace  = namespaceIntern;

  //final val labelIntern = label$$.intern();
  //final def label       = labelIntern;

  final override def typeTag$: Int = 0;

  override def hashCode(): Int = {
    Utility.hashCode(prefix, label, attributes.hashCode(), scope.hashCode(), child);
  }
  /** Return a new element with updated attributes
   *
   *  @param attrs
   *  @return a new symbol with updated attributes
   */
  final def %(attrs: MetaData): Elem =
    Elem(prefix,
         label,
         attrs.append(attributes),
         scope,
         child:_*);

   /* returns concatenation of text(n) for each child n */
   override def text = {
     val sb = new StringBuilder();
     val it = child.elements;
     while(it.hasNext) {
       sb.append(it.next.text);
     }
     sb.toString()
   }

}
