/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml

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

  /** Returns a copy of this unprefixed attribute with the given
   *  next field.
   */
  def copy(next: MetaData) =
    new PrefixedAttribute(pre, key, value, next)

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
}

object PrefixedAttribute {
  def unapply(x: PrefixedAttribute) = Some(x.pre, x.key, x.value, x.next)
}
