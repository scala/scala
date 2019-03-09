package scala.collection.immutable

import org.scalacheck._
import Prop._

object ListMapProperties extends Properties("immutable.ListMap") {

  type K = Int
  type V = Int
  type T = (K, V)

  property("from(linkedHashMap) == from(linkedHashMap.toList)") = forAll { m: Map[K, V] =>
    val lhm = m.to(collection.mutable.LinkedHashMap)
    ListMap.from(lhm) ?= ListMap.from(lhm.toList)
  }

  property("from(map) == from(map.toList)") = forAll { m: Map[K, V] =>
    ListMap.from(m) ?= ListMap.from(m.toList)
  }
  property("from(map.view) == from(map)") = forAll { m: Map[K, V] =>
    ListMap.from(m.view) ?= ListMap.from(m)
  }
}
