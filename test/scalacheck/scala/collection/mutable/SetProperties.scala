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


object SetProperties extends Properties("mutable.Set") {

  type Elem = Int

  /** returns a set that certainly does not override `filterInPlace` implementation */
  def testSet(elems: collection.immutable.Set[Elem]): Set[Elem] = new Set[Elem] {
    override protected[this] def className: String = "TestSet"
    private[this] var _elems = elems
    override def iterator: Iterator[Elem] = _elems.iterator
    override def clear(): Unit = _elems = collection.immutable.Set.empty
    override def subtractOne(elem: Elem): this.type = { _elems -= elem; this }
    override def addOne(elem: Elem): this.type = { _elems += elem; this }
    override def contains(elem: Elem): Boolean = { _elems.contains(elem) }
  }

  implicit val arbSet: Arbitrary[Set[Elem]] =
    Arbitrary {
      for {
        elems <- arbitrary[collection.immutable.HashSet[Elem]]
        set <- oneOf(
          elems.to(HashSet),
          elems.to(TreeSet),
          elems.to(LinkedHashSet),
          testSet(elems)
        )
      } yield set
    }

  property("filterInPlace(p)") = forAll { (set: Set[Elem], p: Elem => Boolean) =>
    val expected: collection.Set[Elem] = set.to(collection.immutable.HashSet).filter(p)
    set.filterInPlace(p)
    (set: collection.Set[Elem]) ?= expected
  }
}
