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


import generic._


/** A trait for sets which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenSet[A]
extends GenSetLike[A, GenSet[A]]
   with GenIterable[A]
   with GenericSetTemplate[A, GenSet]
{
  override def companion: GenericCompanion[GenSet] = GenSet
  def seq: Set[A]
}


object GenSet extends GenTraversableFactory[GenSet] {
  implicit def canBuildFrom[A] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A] = Set.newBuilder
  private[collection] def setEquals[A1, A2](thisSet: GenSetLike[A1, _], thatSet: GenSet[A2]): Boolean = {
    (thisSet eq thatSet) ||
      (thatSet canEqual thisSet) &&
        (thisSet.size == thatSet.size) &&
        (try thisSet subsetOf thatSet.asInstanceOf[GenSet[A1]]
        catch { case ex: ClassCastException => false })
  }
}

