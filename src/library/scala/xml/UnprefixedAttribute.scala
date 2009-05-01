/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

/** Unprefixed attributes have the null namespace, and no prefix field
 *
 *  @author Burak Emir
 */
class UnprefixedAttribute(
  val key: String,
  val value: Seq[Node],
  next1: MetaData)
extends Attribute
{
  val next = if (value ne null) next1 else next1.remove(key)

  /** same as this(key, Text(value), next) */
  def this(key: String, value: String, next: MetaData) =
    this(key, if (value ne null) Text(value) else {val z:NodeSeq=null;z}, next)

  /** same as this(key, value.get, next), or no attribute if value is None */
  def this(key: String, value: Option[Seq[Node]], next: MetaData) =
    this(key, if (!value.isEmpty) value.get else {val z:NodeSeq=null;z}, next)

  /** returns a copy of this unprefixed attribute with the given next field*/
  def copy(next: MetaData) =
    new UnprefixedAttribute(key, value, next)

  def equals1(m: MetaData) =
    !m.isPrefixed && (m.key == key) && (m.value sameElements value)

  /** returns null */
  final def getNamespace(owner: Node): String =
    null

  /**
   * Gets value of unqualified (unprefixed) attribute with given key, null if not found
   *
   * @param  key
   * @return value as Seq[Node] if key is found, null otherwise
   */
  def apply(key: String): Seq[Node] =
    if (key == this.key) value else next(key)

  /**
   * Forwards the call to next (because caller looks for prefixed attribute).
   *
   * @param  namespace
   * @param  scope
   * @param  key
   * @return ..
   */
  def apply(namespace: String, scope: NamespaceBinding, key: String): Seq[Node] =
    next(namespace, scope, key)

  /** returns the hashcode.
   */
  override def hashCode() =
    key.hashCode() * 7 + { if(value ne null) value.hashCode() * 53 else 0 } + next.hashCode()

  /** returns false */
  final def isPrefixed = false

  /** appends string representation of only this attribute to stringbuffer.
   *
   *  @param sb ..
   */
  def toString1(sb: StringBuilder): Unit = if (value ne null) {
    sb.append(key)
    sb.append('=')
    val sb2 = new StringBuilder()
    Utility.sequenceToXML(value, TopScope, sb2, true)
    Utility.appendQuoted(sb2.toString(), sb)
  }

  def wellformed(scope: NamespaceBinding): Boolean =
    (null == next(null, scope, key)) && next.wellformed(scope)

  def remove(key: String) =
    if (this.key == key) next else copy(next.remove(key))

  def remove(namespace: String, scope: NamespaceBinding, key: String): MetaData =
    next.remove(namespace, scope, key)

}
object UnprefixedAttribute {
  def unapply(x: UnprefixedAttribute) = Some(x.key, x.value, x.next)
}
