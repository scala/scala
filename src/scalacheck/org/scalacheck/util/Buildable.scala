/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2017 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import scala.collection.{mutable, Map => _, _}

trait Buildable[T,C] extends Serializable {
  def builder: mutable.Builder[T,C]
  def fromIterable(it: Iterable[T]): C = {
    val b = builder
    b ++= it
    b.result()
  }
}

/**
  * Factory instances implementing Serializable, so that the objects capturing those can be
  * serializable too.
  */
object SerializableCanBuildFroms {

  implicit def listFactory[T]: Factory[T, List[T]] =
    new Factory[T, List[T]] with Serializable {
      def fromSpecific(source: IterableOnce[T]): List[T] = List.from(source)
      def newBuilder: mutable.Builder[T, List[T]] = List.newBuilder[T]
    }

  implicit def bitsetFactory[T]: Factory[Int, BitSet] =
    new Factory[Int, BitSet] with Serializable {
      def fromSpecific(source: IterableOnce[Int]) = BitSet.fromSpecific(source)
      def newBuilder: mutable.Builder[Int, BitSet] = BitSet.newBuilder
    }

  implicit def mapFactory[T, U]: Factory[(T, U), Map[T, U]] =
    new Factory[(T, U), Map[T, U]] with Serializable {
      def fromSpecific(source: IterableOnce[(T, U)]) = Map.from(source)
      def newBuilder: mutable.Builder[(T, U), Map[T, U]] = Map.newBuilder[T, U]
    }

}

object Buildable {

  implicit def buildableFactory[T,C](implicit f: Factory[T,C]) =
    new Buildable[T,C] {
      def builder = f.newBuilder
    }

  import java.util.ArrayList
  implicit def buildableArrayList[T] = new Buildable[T,ArrayList[T]] {
    def builder = new ArrayListBuilder[T]
  }

}
/*
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
*/
