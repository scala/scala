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

/** An extractor used to head/tail deconstruct sequences. */
object +: {
  def unapply[T,Coll <: SeqLike[T, Coll]](
      t: Coll with SeqLike[T, Coll]): Option[(T, Coll)] =
    if(t.isEmpty) None
    else Some(t.head -> t.tail)
}

/** An extractor used to init/last deconstruct sequences. */
object :+ {
  /** Splits a sequence into init :+ last.
   * @return Some((init, last)) if sequence is non-empty. None otherwise.
   */
  def unapply[T,Coll <: SeqLike[T, Coll]](
      t: Coll with SeqLike[T, Coll]): Option[(Coll, T)] =
    if(t.isEmpty) None
    else Some(t.init -> t.last)
}

// Dummy to fool ant
private abstract class SeqExtractors
