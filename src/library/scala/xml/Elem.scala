/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

/** This singleton object contains the apply and unapplySeq methods for convenient construction and
 *  deconstruction. It is possible to deconstruct any Node instance (that is not a SpecialNode or
 *  a Group) using the syntax
 * <code> case Elem(prefix, label, attribs, scope, child @ _*) => ... </code>
 *
 * Copyright 2008 Google Inc. All Rights Reserved.
 * @author Burak Emir <bqe@google.com>
 */
object Elem {

  def apply(prefix: String,label: String, attributes: MetaData, scope: NamespaceBinding, child: Node*) =
    new Elem(prefix,label,attributes,scope,child:_*)

  def unapplySeq(n:Node) = if (n.isInstanceOf[SpecialNode] || n.isInstanceOf[Group]) None else
    Some(Tuple5(n.prefix, n.label, n.attributes, n.scope, n.child))


}
/** The case class <code>Elem</code> extends the <code>Node</code> class,
 *  providing an immutable data object representing an XML element.
 *
 *  @param prefix namespace prefix (may be null, but not the empty string)
 *  @param label the element name
 *  @param attribute the attribute map
 *  @param scope the scope containing the namespace bindings
 *  @param child the children of this node
 *
 * Copyright 2008 Google Inc. All Rights Reserved.
 * @author Burak Emir <bqe@google.com>
 */
// "val" is redundant for non-overriding arguments
@serializable class Elem(override val prefix: String,
                val label: String,
                override val attributes: MetaData,
                override val scope: NamespaceBinding,
                val child: Node*) extends Node {

  if ((null != prefix) && 0 == prefix.length())
    throw new IllegalArgumentException("prefix of zero length, use null instead")

  if (null == scope)
    throw new IllegalArgumentException("scope is null, try xml.TopScope for empty scope")

  //@todo: copy the children,
  //  setting namespace scope if necessary
  //  cleaning adjacent text nodes if necessary

  final override def typeTag$: Int = 0

  override def hashCode(): Int =
    Utility.hashCode(prefix, label, attributes.hashCode(), scope.hashCode(), child)

  /** Returns a new element with updated attributes, resolving namespace uris from this element's scope.
   *  See MetaData.update for details.
   *  @param  updates MetaData with new and updated attributes
   *  @return a new symbol with updated attributes
   */
  final def %(updates: MetaData): Elem =
    Elem(prefix,
         label,
         MetaData.update(attributes, scope, updates),
         scope,
         child:_*)

   /** Returns concatenation of <code>text(n)</code> for each child
    *  <code>n</code>.
    */
   override def text = {
     val sb = new StringBuilder()
     val it = child.iterator
     while (it.hasNext)
       sb.append(it.next.text)
     sb.toString()
   }

}
