/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import collection.Seq
import collection.mutable.StringBuilder


/** prefixed attributes always have a non-null namespace.
 *
 *  @param pre   ...
 *  @param key   ...
 *  @param value the attribute value, which may not be null
 *  @param next  ...
 */
class PrefixedAttribute(
  val pre: String,
  val key: String,
  val value: Seq[Node],
  val next: MetaData)
extends Attribute
{
  if (value eq null)
    throw new UnsupportedOperationException("value is null")

  /** same as this(key, Utility.parseAttributeValue(value), next) */
  def this(pre: String, key: String, value: String, next: MetaData) =
    this(pre, key, Text(value), next)

  /*
   // the problem here is the fact that we cannot remove the proper attribute from
   // next, and thus cannot guarantee that hashcodes are computed properly
  def this(pre: String, key: String, value: scala.AllRef, next: MetaData) =
    throw new UnsupportedOperationException("can't construct prefixed nil attributes")
 */

  /** Returns a copy of this unprefixed attribute with the given
   *  next field.
   */
  def copy(next: MetaData) =
    new PrefixedAttribute(pre, key, value, next)

  def equals1(m: MetaData) =
     (m.isPrefixed &&
      (m.asInstanceOf[PrefixedAttribute].pre == pre) &&
      (m.key == key) && (m.value sameElements value))

  def getNamespace(owner: Node) =
    owner.getNamespace(pre)

  /** forwards the call to next (because caller looks for unprefixed attribute */
  def apply(key: String): Seq[Node] = next(key)

  /** gets attribute value of qualified (prefixed) attribute with given key
   */
  def apply(namespace: String, scope: NamespaceBinding, key: String): Seq[Node] = {
    if (key == this.key && scope.getURI(pre) == namespace)
      value
    else
      next(namespace, scope, key)
  }

  /** returns true */
  final def isPrefixed = true

  /** returns the hashcode.
   */
  override def hashCode() =
    pre.hashCode() * 41 + key.hashCode() * 7 + next.hashCode()


  /** appends string representation of only this attribute to stringbuffer */
  def toString1(sb:StringBuilder): Unit = if(value ne null) {
    sb.append(pre)
    sb.append(':')
    sb.append(key)
    sb.append('=')
    val sb2 = new StringBuilder()
    Utility.sequenceToXML(value, TopScope, sb2, true)
    Utility.appendQuoted(sb2.toString(), sb)
  }

  def wellformed(scope: NamespaceBinding): Boolean =
    (null == next(scope.getURI(pre), scope, key) &&
     next.wellformed(scope))

  def remove(key: String) =
    copy(next.remove(key))

  def remove(namespace: String, scope: NamespaceBinding, key: String): MetaData =
    if (key == this.key && scope.getURI(pre) == namespace)
      next
    else
      next.remove(namespace, scope, key)

}
object PrefixedAttribute {
  def unapply(x: PrefixedAttribute) = Some(x.pre, x.key, x.value, x.next)
}
