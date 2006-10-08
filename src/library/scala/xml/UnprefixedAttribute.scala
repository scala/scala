/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml

import compat.StringBuilder

/** unprefixed attributes have the null namespace
 */
class UnprefixedAttribute(val key: String, val value: Seq[Node], val next: MetaData) extends MetaData {

  /** same as this(key, Utility.parseAttributeValue(value), next) */
  def this(key: String, value: String, next: MetaData) =
    this(key, Utility.parseAttributeValue(value), next)

  /* verify that value is a proper attribute value (references, no &lt)
  Utility.checkAttributeValue(value) match {
    case null =>
    case msg  => throw new MalformedAttributeException(msg)
  }
*/

  /** returns a copy of this unprefixed attribute with the given next field*/
  def copy(next: MetaData) =
    new UnprefixedAttribute(key, value, next)

  def equals1(m:MetaData) =
    !m.isPrefixed && (m.key == key) && (m.value sameElements value)

  /** returns null */
  final def getNamespace(owner: Node): String =
    null

  /**
   * Gets value of unqualified (unprefixed) attribute with given key.
   *
   * @param  key
   * @return ..
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

  override def hashCode() =
    key.hashCode() * 7 + value.hashCode() * 53 + next.hashCode()

  /** returns false */
  final def isPrefixed = false

  def toString1(sb:StringBuilder): Unit = {
    sb.append(key)
    sb.append('=')
    val sb2 = new StringBuilder()
    for (val c <- value) {
      Utility.toXML(c, TopScope, sb2, true)
    }
    Utility.appendQuoted(sb2.toString(), sb)
  }

  def wellformed(scope: NamespaceBinding): Boolean =
    (null == next(null, scope, key)) && next.wellformed(scope)

}

