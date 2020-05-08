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
package immutable

import generic._
import mutable.Builder
import parallel.immutable.ParSeq
import scala.math.Ordering

/** A subtrait of `collection.Seq` which represents sequences
 *  that are guaranteed immutable.
 *
 *  $seqInfo
 *  @define Coll `immutable.Seq`
 *  @define coll immutable sequence
 */
trait Seq[+A] extends Iterable[A]
//                      with GenSeq[A]
                      with scala.collection.Seq[A]
                      with GenericTraversableTemplate[A, Seq]
                      with SeqLike[A, Seq[A]]
                      with Parallelizable[A, ParSeq[A]]
{
  override def companion: GenericCompanion[Seq] = Seq
  override def toSeq: Seq[A] = this
  override def seq: Seq[A] = this
  protected[this] override def parCombiner = ParSeq.newCombiner[A] // if `immutable.SeqLike` gets introduced, please move this there!
  override def sorted[B >: A](implicit ord: Ordering[B]): this.type = {
    val len = this.length
    if (len < 2 || (len == 2 && ord.compare(apply(0), apply(1)) <= 0)) this
    else if (len == 2) {
      //we know the order should be reversed
      //and some implementations have optimised methods for this
      reverse.asInstanceOf[this.type]
    } else {
      val arr = toArray[Any].asInstanceOf[Array[AnyRef]]
      java.util.Arrays.sort(arr, ord.asInstanceOf[Ordering[Object]])
      fromAnyRefArray(arr).asInstanceOf[this.type]
    }
  }

}

/** $factoryInfo
 *  @define Coll `immutable.Seq`
 *  @define coll immutable sequence
 */
object Seq extends SeqFactory[Seq] {
  /** genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Seq[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Seq[A]] = new mutable.ListBuffer
}
