/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

/** Class used internally for default map model.
 *  @since 2.3
 */
@SerialVersionUID(-3856907690109104385L)
final class DefaultEntry[A, B](val key: A, var value: B)
      extends HashEntry[A, DefaultEntry[A, B]] with Serializable
{
  override def toString = chainString

  def chainString = {
    "(kv: " + key + ", " + value + ")" + (if (next != null) " -> " + next.toString else "")
  }
}
