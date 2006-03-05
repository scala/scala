/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml;


/** prefixed attributes always have a non-null namespace
 */
class PrefixedAttribute(val pre: String,
                        val key: String,
                        val value: String,
                        val next: MetaData) extends MetaData {

  // verify that value is a proper attribute value (references, no &lt;)
  Utility.checkAttributeValue(value) match {
    case null => ;
    case msg  => throw new MalformedAttributeException(msg);
  }

  /** Returns a copy of this unprefixed attribute with the given
   *  next field.
   */
  def copy(next: MetaData) =
    new PrefixedAttribute(pre, key, value, next);

  //** duplicates the MetaData (deep copy), not preserving order */
  //def deepCopy: MetaData = deepCopy(null);

  //** duplicates the MetaData (deep copy), prepending it to tail */
  /*
  def deepCopy(tail: MetaData): MetaData = {
    val md = copy(tail);
    if (null == next)
      md
    else
      next.deepCopy(md)
  }
  */

  def equals1(m: MetaData) =
     (m.isPrefixed &&
      (m.asInstanceOf[PrefixedAttribute].pre == pre) &&
      (m.key == key) && (m.value == value));

  def getNamespace(owner: Node) =
    owner.getNamespace(pre);

  /** forwards the call to next */
  def getValue(key: String): String = next.getValue(key);

  /** gets attribute value of qualified (prefixed) attribute with given key
   */
  def getValue(namespace: String, scope: NamespaceBinding, key: String): String = {
    if (key == this.key && scope.getURI(pre) == namespace)
      value
    else
      next.getValue(namespace, scope, key);
  }

  /** returns true */
  final def isPrefixed = true;

  override def hashCode() =
    pre.hashCode() * 41 + key.hashCode() * 7 + value.hashCode() * 3 + next.hashCode();


  def toString1(sb:StringBuffer): Unit = {
    sb.append(pre);
    sb.append(':');
    sb.append(key);
    sb.append('=');
    Utility.appendQuoted(value, sb);
  }

  def wellformed(scope: NamespaceBinding): Boolean = {
    (null == next.getValue(scope.getURI(pre), scope, key)
     && next.wellformed(scope));
  }

}

