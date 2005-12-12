/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml;

/** unprefixed attributes have the null namespace
 */
class UnprefixedAttribute(val key: String, val value: Any, val next: MetaData) extends MetaData {

  /*
  // verify that value is a proper attribute value (references, no &lt;)
  // this should happen before the attribute is constructed.
  Utility.checkAttributeValue(value) match {
    case null => ;
    case msg  => throw new MalformedAttributeException(msg);
  }
*/
  /** returns a copy of this unprefixed attribute with the given next field*/
  def copy(next: MetaData) =
    new UnprefixedAttribute(key, value, next);

  def equals1(m:MetaData) =
    !m.isPrefixed && (m.key == key) && (m.value == value);

  /** returns null */
  final def getNamespace(owner: Node): String =
    null;

  /**
   * Gets value of unqualified (unprefixed) attribute with given key.
   *
   * @param  key
   * @return ..
   */
  def getValue(key: String): Any =
    if (key == this.key) value else next.getValue(key);

  /**
   * Forwards the call to next.
   *
   * @param  namespace
   * @param  scope
   * @param  key
   * @return ..
   */
  def getValue(namespace: String, scope: NamespaceBinding, key: String): Any =
    next.getValue(namespace, scope, key);

  override def hashCode() =
    key.hashCode() * 7 + value.hashCode() * 53 + next.hashCode();

  /** returns false */
  final def isPrefixed = false;

  def toString1(sb:StringBuffer): Unit = {
    sb.append(key);
    sb.append('=');
    Utility.appendAttributeValue(value.toString(), sb);
  }

  def wellformed(scope: NamespaceBinding): Boolean =
    (null == next.getValue(null, scope, key)) && next.wellformed(scope);

}

