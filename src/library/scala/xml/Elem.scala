/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** This singleton object contains the `apply` and `unapplySeq` methods for
 *  convenient construction and deconstruction. It is possible to deconstruct
 *  any `Node` instance (that is not a `SpecialNode` or a `Group`) using the
 *  syntax `case Elem(prefix, label, attribs, scope, child @ _*) => ...`
 *
 *  Copyright 2008 Google Inc. All Rights Reserved.
 *  @author Burak Emir <bqe@google.com>
 */
object Elem {
  /** Build an Elem, setting its minimizeEmpty property to <code>true</code> if it has no children.  Note that this
   *  default may not be exactly what you want, as some XML dialects don't permit some elements to be minimized.
   *
   * @deprecated This factory method is retained for backward compatibility; please use the other one, with which you
   *             can specify your own preference for minimizeEmpty.
   */
  @deprecated("Use the other apply method in this object", "2.10.0")
  def apply(prefix: String, label: String, attributes: MetaData, scope: NamespaceBinding, child: Node*): Elem =
    apply(prefix, label, attributes, scope, child.isEmpty, child: _*)

  def apply(prefix: String, label: String, attributes: MetaData, scope: NamespaceBinding, minimizeEmpty: Boolean, child: Node*): Elem =
    new Elem(prefix, label, attributes, scope, minimizeEmpty, child: _*)

  def unapplySeq(n: Node) = n match {
    case _: SpecialNode | _: Group  => None
    case _                          => Some((n.prefix, n.label, n.attributes, n.scope, n.child))
  }
}

/** The case class `Elem` extends the `Node` class,
 *  providing an immutable data object representing an XML element.
 *
 *  @param prefix        namespace prefix (may be null, but not the empty string)
 *  @param label         the element name
 *  @param attributes1   the attribute map
 *  @param scope         the scope containing the namespace bindings
 *  @param minimizeEmpty `true` if this element should be serialized as minimized (i.e. "&lt;el/&gt;") when
 *                       empty; `false` if it should be written out in long form.
 *  @param child         the children of this node
 *
 *  Copyright 2008 Google Inc. All Rights Reserved.
 *  @author Burak Emir <bqe@google.com>
 */
class Elem(
  override val prefix: String,
  val label: String,
  attributes1: MetaData,
  override val scope: NamespaceBinding,
  val minimizeEmpty: Boolean,
  val child: Node*)
extends Node with Serializable
{
  @deprecated("This constructor is retained for backward compatibility. Please use the primary constructor, which lets you specify your own preference for `minimizeEmpty`.", "2.10.0")
  def this(prefix: String, label: String, attributes: MetaData, scope: NamespaceBinding, child: Node*) = {
    this(prefix, label, attributes, scope, child.isEmpty, child: _*)
  }

  final override def doCollectNamespaces = true
  final override def doTransform         = true

  override val attributes = MetaData.normalize(attributes1, scope)

  if (prefix == "")
    throw new IllegalArgumentException("prefix of zero length, use null instead")

  if (scope == null)
    throw new IllegalArgumentException("scope is null, use scala.xml.TopScope for empty scope")

  //@todo: copy the children,
  //  setting namespace scope if necessary
  //  cleaning adjacent text nodes if necessary

  override protected def basisForHashCode: Seq[Any] =
    prefix :: label :: attributes :: child.toList

  /** Returns a new element with updated attributes, resolving namespace uris
   *  from this element's scope. See MetaData.update for details.
   *
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
    minimizeEmpty: Boolean = this.minimizeEmpty,
    child: Seq[Node] = this.child.toSeq
  ): Elem = Elem(prefix, label, attributes, scope, minimizeEmpty, child: _*)

  /** Returns concatenation of `text(n)` for each child `n`.
   */
  override def text = (child map (_.text)).mkString
}
