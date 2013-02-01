/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


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
  final val pre = null
  val next = if (value ne null) next1 else next1.remove(key)

  /** same as this(key, Text(value), next), or no attribute if value is null */
  def this(key: String, value: String, next: MetaData) =
    this(key, if (value ne null) Text(value) else null: NodeSeq, next)

  /** same as this(key, value.get, next), or no attribute if value is None */
  def this(key: String, value: Option[Seq[Node]], next: MetaData) =
    this(key, value.orNull, next)

  /** returns a copy of this unprefixed attribute with the given next field*/
  def copy(next: MetaData) = new UnprefixedAttribute(key, value, next)

  final def getNamespace(owner: Node): String = null

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
}
object UnprefixedAttribute {
  def unapply(x: UnprefixedAttribute) = Some((x.key, x.value, x.next))
}
