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
package generic
import scala.language.higherKinds

/** A template for companion objects of Seq and subclasses thereof.
 *
 *  @since 2.8
 */
abstract class SeqFactory[CC[X] <: Seq[X] with GenericTraversableTemplate[X, CC]]
extends GenSeqFactory[CC] with TraversableFactory[CC] {

  /** This method is called in a pattern match { case Seq(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Seq, otherwise none
   */
  def unapplySeq[A](x: CC[A]): Some[CC[A]] = Some(x)

}

