/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml

/** Attribute defines the interface shared by both
 *  PrefixedAttribute and UnprefixedAttribute
 */

object Attribute {
  def unapply(x: Attribute) = x match {
    case PrefixedAttribute(_, key, value, next) => Some(key, value, next)
    case UnprefixedAttribute(key, value, next)  => Some(key, value, next)
    case _                                      => None
  }

  /** Convenience functions which choose Un/Prefixedness appropriately */
  def apply(key: String, value: Seq[Node], next: MetaData): Attribute =
    new UnprefixedAttribute(key, value, next)

  def apply(pre: String, key: String, value: String, next: MetaData): Attribute =
    if (pre == null || pre == "") new UnprefixedAttribute(key, value, next)
    else new PrefixedAttribute(pre, key, value, next)

  def apply(pre: String, key: String, value: Seq[Node], next: MetaData): Attribute =
    if (pre == null || pre == "") new UnprefixedAttribute(key, value, next)
    else new PrefixedAttribute(pre, key, value, next)

  def apply(pre: Option[String], key: String, value: Seq[Node], next: MetaData): Attribute =
    pre match {
      case None     => new UnprefixedAttribute(key, value, next)
      case Some(p)  => new PrefixedAttribute(p, key, value, next)
    }
}

abstract trait Attribute extends MetaData
{
  def pre: String        // will be null if unprefixed
  val key: String
  val value: Seq[Node]
  val next: MetaData

  def apply(key: String): Seq[Node]
  def apply(namespace: String, scope: NamespaceBinding, key: String): Seq[Node]
  def copy(next: MetaData): Attribute

  def remove(key: String) =
    if (!isPrefixed && this.key == key) next
    else copy(next remove key)

  def remove(namespace: String, scope: NamespaceBinding, key: String) =
    if (isPrefixed && this.key == key && (scope getURI pre) == namespace) next
    else next.remove(namespace, scope, key)

  def isPrefixed: Boolean = pre != null
  def getNamespace(owner: Node): String
  def wellformed(scope: NamespaceBinding): Boolean = {
    val arg = if (isPrefixed) scope getURI pre else null
    (next(arg, scope, key) == null) && (next wellformed scope)
  }

  override def canEqual(other: Any) = other match {
    case _: Attribute   => true
    case _              => false
  }
  override def strict_==(other: Equality) = other match {
    case x: Attribute   => (pre == x.pre) && (key == x.key) && (value sameElements x.value)
    case _              => false
  }
  override def basisForHashCode = List(pre, key, value)

  /** Appends string representation of only this attribute to stringbuffer.
   */
  def toString1(sb: StringBuilder) {
    if (value == null)
      return
    if (isPrefixed)
      sb append pre append ':'

    sb append key append '='
    val sb2 = new StringBuilder()
    Utility.sequenceToXML(value, TopScope, sb2, true)
    Utility.appendQuoted(sb2.toString(), sb)
  }
}
