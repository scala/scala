/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml

import collection.Seq
import collection.mutable.StringBuilder


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
  val key: String
  val value: Seq[Node]
  val next: MetaData

  def apply(key: String): Seq[Node]
  def apply(namespace: String, scope: NamespaceBinding, key: String): Seq[Node]
  def copy(next: MetaData): Attribute
  def remove(key: String): MetaData
  def remove(namespace: String, scope: NamespaceBinding, key: String): MetaData

  def isPrefixed: Boolean
  def getNamespace(owner: Node): String
  def wellformed(scope: NamespaceBinding): Boolean

  def equals1(m: MetaData): Boolean
  def toString1(sb: StringBuilder): Unit
}
