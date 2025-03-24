/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package sys

import scala.collection.mutable

/** The internal implementation of scala.sys.Prop.
 */
private[sys] class PropImpl[+T](val key: String, valueFn: String => T) extends Prop[T] {
  def value: T = if (isSet) valueFn(get) else zero
  def isSet    = underlying contains key
  def set(newValue: String): String = {
    val old = if (isSet) get else null
    underlying(key) = newValue
    old
  }
  def setValue[T1 >: T](newValue: T1): T = {
    val old = value
    if (newValue == null) set(null)
    else set("" + newValue)
    old
  }
  def get: String =
    if (isSet) underlying.getOrElse(key, "")
    else ""

  def clear(): Unit = underlying -= key
  def option: Option[T] = if (isSet) Some(value) else None
  def or[T1 >: T](alt: => T1): T1 = if (isSet) value else alt

  /** The underlying property map, in our case always sys.props */
  protected def underlying: mutable.Map[String, String] = scala.sys.props
  protected def zero: T = null.asInstanceOf[T]
  private def getString = if (isSet) "currently: " + get else "unset"
  override def toString = "%s (%s)".format(key, getString)
}

private[sys] abstract class CreatorImpl[+T](f: String => T) extends Prop.Creator[T] {
  def apply(key: String): Prop[T] = new PropImpl[T](key, f)
}

