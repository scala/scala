/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import collection._

trait Buildable[T,C[_]] {
  def builder: mutable.Builder[T,C[T]]
  def fromIterable(it: Traversable[T]): C[T] = {
    val b = builder
    b ++= it
    b.result()
  }
}

trait Buildable2[T,U,C[_,_]] {
  def builder: mutable.Builder[(T,U),C[T,U]]
  def fromIterable(it: Traversable[(T,U)]): C[T,U] = {
    val b = builder
    b ++= it
    b.result()
  }
}

object Buildable {
  import generic.CanBuildFrom

  implicit def buildableCanBuildFrom[T, C[_]](implicit c: CanBuildFrom[C[_], T, C[T]]) = 
    new Buildable[T, C] {
      def builder = c.apply
    }

  import java.util.ArrayList
  implicit def buildableArrayList[T] = new Buildable[T,ArrayList] {
    def builder = new mutable.Builder[T,ArrayList[T]] {
      val al = new ArrayList[T]
      def +=(x: T) = {
        al.add(x)
        this
      }
      def clear() = al.clear()
      def result() = al
    }
  }

}

object Buildable2 {
  
  implicit def buildableMutableMap[T,U] = new Buildable2[T,U,mutable.Map] {
    def builder = mutable.Map.newBuilder
  }

  implicit def buildableImmutableMap[T,U] = new Buildable2[T,U,immutable.Map] {
    def builder = immutable.Map.newBuilder
  }

  implicit def buildableMap[T,U] = new Buildable2[T,U,Map] {
    def builder = Map.newBuilder
  }

  implicit def buildableImmutableSortedMap[T: Ordering, U] = new Buildable2[T,U,immutable.SortedMap] {
    def builder = immutable.SortedMap.newBuilder
  }

  implicit def buildableSortedMap[T: Ordering, U] = new Buildable2[T,U,SortedMap] {
    def builder = SortedMap.newBuilder
  }

}
