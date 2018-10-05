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
package math

/** A class for partially ordered data.
 *
 *  @author  Martin Odersky
 */
trait PartiallyOrdered[+A] {

  /** Result of comparing `'''this'''` with operand `that`.
   *  Returns `None` if operands are not comparable.
   *  If operands are comparable, returns `Some(x)` where
   *  - `x < 0`    iff   `'''this''' &lt; that`
   *  - `x == 0`   iff   `'''this''' == that`
   *  - `x > 0`    iff   `'''this''' &gt; that`
   */
  def tryCompareTo [B >: A <% PartiallyOrdered[B]](that: B): Option[Int]

  def <  [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x < 0 => true
      case _ => false
    }
  def >  [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x > 0 => true
      case _ => false
    }
  def <= [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x <= 0 => true
      case _ => false
    }
  def >= [B >: A <% PartiallyOrdered[B]](that: B): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x >= 0 => true
      case _ => false
    }
}
