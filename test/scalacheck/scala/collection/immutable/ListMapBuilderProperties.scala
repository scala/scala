package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Gen._

object ListMapBuilderProperties extends Properties("immutable.ListMapBuilder") {

  type K = Int
  type V = Int

  sealed trait Transform

  object Transform {
    case class AddOne(kv: (K, V)) extends Transform
    case class AddAllList(kvs: List[(K, V)]) extends Transform
    case class AddAllMap(kvs: Map[K, V]) extends Transform
    case object Clear extends Transform
  }



  val transformGen: Gen[List[Transform]] =
    listOf(oneOf(
      Arbitrary.arbitrary[(K, V)].map(Transform.AddOne),
      Arbitrary.arbitrary[List[(K, V)]].map(Transform.AddAllList),
      Arbitrary.arbitrary[Map[K, V]].map(Transform.AddAllMap),
      const(Transform.Clear)
    ))

  def interpret(transforms: Seq[Transform], builder: () => collection.mutable.Builder[(K, V), Map[K, V]]): Map[K, V] =
    transforms.foldLeft(builder()) {
      case (b, Transform.AddOne(kv)) => b.addOne(kv)
      case (b, Transform.AddAllList(kvs)) => b.addAll(kvs)
      case (b, Transform.AddAllMap(kvs)) => b.addAll(kvs)
      case (b, Transform.Clear) => b.clear(); b
    }.result()


  property("Multiple transforms on ListMapBuilder give same result as HashMapBuilder") =
    forAll(transformGen) { transforms: Seq[Transform] =>
      interpret(transforms, () => ListMap.newBuilder) ?= interpret(transforms, () => HashMap.newBuilder)
  }

}
