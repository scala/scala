/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
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
object Elem
{
  def apply(prefix: String,label: String, attributes: MetaData, scope: NamespaceBinding, child: Node*) =
    new Elem(prefix,label,attributes,scope,child:_*)

  def unapplySeq(n: Node) = n match {
    case _: SpecialNode | _: Group  => None
    case _                          => Some((n.prefix, n.label, n.attributes, n.scope, n.child))
  }
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
@serializable
class Elem(
  override val prefix: String,
  val label: String,
  override val attributes: MetaData,
  override val scope: NamespaceBinding,
  val child: Node*)
extends Node
{
  final override def doCollectNamespaces = true
  final override def doTransform         = true

  if (prefix == "")
    throw new IllegalArgumentException("prefix of zero length, use null instead")

  if (scope == null)
    throw new IllegalArgumentException("scope is null, use xml.TopScope for empty scope")

  //@todo: copy the children,
  //  setting namespace scope if necessary
  //  cleaning adjacent text nodes if necessary

  override def basisForHashCode: Seq[Any] = prefix :: label :: attributes :: child.toList

  /** Returns a new element with updated attributes, resolving namespace uris from this element's scope.
   *  See MetaData.update for details.
   *  @param  updates MetaData with new and updated attributes
   *  @return a new symbol with updated attributes
   */
  final def %(updates: MetaData): Elem =
    copy(attributes = MetaData.update(attributes, scope, updates))

  /** Returns a copy of this element with any supplied arguments replacing
   *  this element's value for that field.
   *
   *  @return a new symbol with updated attributes
   */
  def copy(
    prefix: String = this.prefix,
    label: String = this.label,
    attributes: MetaData = this.attributes,
    scope: NamespaceBinding = this.scope,
    child: Seq[Node] = this.child.toSeq
  ): Elem = Elem(prefix, label, attributes, scope, child: _*)

  /** Returns concatenation of <code>text(n)</code> for each child
   *  <code>n</code>.
   */
  override def text = child map (_.text) mkString
}
