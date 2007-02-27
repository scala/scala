/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

object Elem {

  def apply(prefix: String,label: String, attributes: MetaData, scope: NamespaceBinding, child: Node*) =
    new Elem(prefix,label,attributes,scope,child:_*)

  def unapplySeq(n:Node) = if(n.isInstanceOf[SpecialNode]) None else
    Some(Tuple5(n.prefix,n.label,n.attributes,n.scope,n.child))


}
/** The case class <code>Elem</code> extends the <code>Node</code> class,
 *  providing an immutable data object representing an XML element.
 *
 *  @author Burak Emir
 *
 *  @param prefix (may be null)
 *  @param label the element name
 *  @param attribute the attribute map
 *  @param child the children of this node
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

  /** Returns a new element with updated attributes.
   *
   *  @param  attrs ...
   *  @return a new symbol with updated attributes
   */
  final def %(attrs: MetaData): Elem =
    Elem(prefix,
         label,
         attrs.append(attributes),
         scope,
         child:_*)

   /** Returns concatenation of <code>text(n)</code> for each child
    *  <code>n</code>.
    */
   override def text = {
     val sb = new compat.StringBuilder()
     val it = child.elements
     while (it.hasNext)
       sb.append(it.next.text)
     sb.toString()
   }

}
