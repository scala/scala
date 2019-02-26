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

package scala.collection.mutable

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import Gen._


object MapProperties extends Properties("mutable.Map") {

  type K = Int
  type V = Int

  /** returns a map that certainly does not override `filterInPlace` implementation */
  def testMap(elems: collection.immutable.Map[K, V]): Map[K, V] = new Map[K, V] {
    override protected[this] def className: String = "TesMap"
    private[this] var _elems = elems
    override def get(key: K): Option[V] = _elems.get(key)
    override def subtractOne(elem: K): this.type = { _elems -= elem; this }
    override def iterator: Iterator[(K, V)] = _elems.iterator
    override def addOne(elem: (K, V)): this.type = { _elems += elem; this }
  }

  implicit val arbMap: Arbitrary[Map[K, V]] =
    Arbitrary {
      for {
        elems <- arbitrary[collection.immutable.HashMap[K, V]]
        map <- oneOf(
          elems.to(HashMap),
          elems.to(TreeMap),
          elems.to(LinkedHashMap),
          elems.to(ListMap),
          testMap(elems)
        )
      } yield map
    }

  property("filterInPlace(p)") = forAll { (map: Map[K, V], p: (K, V) => Boolean) =>
    val expected: collection.Map[K, V] = map.to(collection.immutable.HashMap).filter(p.tupled)
    map.filterInPlace(p)
    (map: collection.Map[K, V]) ?= expected
  }
}