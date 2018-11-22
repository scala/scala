package scala.collection

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object MapViewProperties extends Properties("MapView") {

  type K = Int
  type V = Int
  type T = (K, V)

  val x = MapView.from(List(1 -> ""))

  property("filter behaves like Map.filter") = forAll { (m: Map[K, V], p: ((K, V)) => Boolean, isFlipped: Boolean) =>
    if (isFlipped)
      m.filterNot(p) == m.view.filterNot(p).toMap
    else
      m.filter(p) == m.view.filter(p).toMap
  }

  property("concat behaves like Map.concat") = forAll { (m0: Map[K, V], m1: Map[K, V]) =>
    val strictStrict = m0 concat m1
    val strictView = m0 concat m1.view
    val viewStrict = (m0.view concat m1).toMap
    val viewView = (m0.view concat m1.view).toMap

    strictStrict == strictView &&
    strictView == viewStrict &&
    viewStrict == viewView
  }
  property("++ behaves like Map.++") = forAll { (m0: Map[K, V], m1: Map[K, V]) =>
    val strictStrict = m0 ++ m1
    val strictView = m0 ++ m1.view
    val viewStrict = (m0.view ++ m1).toMap
    val viewView = (m0.view ++ m1.view).toMap

    strictStrict == strictView &&
      strictView == viewStrict &&
      viewStrict == viewView
  }
  property("partition behaves like Map.partition") = forAll { (m: Map[K, V], p: ((K, V)) => Boolean) =>
    val strict = m.partition(p)
    val (viewA, viewB) = m.view.partition(p)
    strict == (viewA.toMap, viewB.toMap)
  }

}
