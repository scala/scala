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
 *  @param pre   
 *  @param key   
 *  @param value the attribute value
 *  @param next1
 */
class PrefixedAttribute(
  val pre: String,
  val key: String,
  val value: Seq[Node],
  val next1: MetaData)
extends Attribute
{
  val next = if (value ne null) next1 else next1.remove(key)

  /** same as this(pre, key, Text(value), next), or no attribute if value is null */
  def this(pre: String, key: String, value: String, next: MetaData) =
    this(pre, key, if (value ne null) Text(value) else null: NodeSeq, next)

  /** same as this(pre, key, value.get, next), or no attribute if value is None */
  def this(pre: String, key: String, value: Option[Seq[Node]], next: MetaData) =
    this(pre, key, value.orNull, next)

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
  def unapply(x: PrefixedAttribute) = Some((x.pre, x.key, x.value, x.next))
}
