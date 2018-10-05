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


/** A trait for all iterable collections which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenIterable[+A]
extends GenIterableLike[A, GenIterable[A]]
   with GenTraversable[A]
   with GenericTraversableTemplate[A, GenIterable]
{
  def seq: Iterable[A]
  override def companion: GenericCompanion[GenIterable] = GenIterable
}


object GenIterable extends GenTraversableFactory[GenIterable] {
  implicit def canBuildFrom[A] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A] = Iterable.newBuilder
}

