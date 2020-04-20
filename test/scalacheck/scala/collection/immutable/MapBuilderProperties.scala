package scala.collection.immutable

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object MapBuilderProperties extends Properties("immutable.MapBuilder") {
  case class MapPair(m1: Map[Int, Int], m2: Map[Int, Int]) {
    lazy val s1 = m1.iterator.map(_._1).toSet
    lazy val s2 = m2.iterator.map(_._1).toSet
  }
  implicit def MapPairArbitray: Arbitrary[MapPair] = Arbitrary(Arbitrary.arbitrary[IndexedSeq[Int]].flatMap { is =>
    val all = Map(is.map(x => (x -> x)): _*)
    val subset2 = all.filter(_._1 % 2 == 0)
    val subset3 = all.filter(_._1 % 3 == 0)
    val subset5 = all.filter(_._1 % 5 == 0)
    val combinations = List(all, subset2, subset3, subset5).combinations(2).map {
      case List(m1, m2) => MapPair(m1, m2)
    }.toSeq
    Gen.oneOf(combinations)
  })

  property("map bulk append exiting collection") = forAll { (mp: MapPair) =>
    val reference = collection.mutable.Map[Int, Int]()
    val builder = Map.newBuilder[Int, Int]
    builder ++= mp.m1
    builder ++= mp.m2
    reference ++= mp.m1
    reference ++= mp.m2
    builder.result == reference
  }

  property("set bulk append exiting collection") = forAll { (mp: MapPair) =>
    val reference = collection.mutable.Set[Int]()
    val builder = Set.newBuilder[Int]
    builder ++= mp.s1
    builder ++= mp.s2
    reference ++= mp.s1
    reference ++= mp.s2
    builder.result == reference
  }
}

