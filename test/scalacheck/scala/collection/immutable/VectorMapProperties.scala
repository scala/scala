package scala.collection.immutable

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._

object VectorMapProperties extends Properties("immutable.VectorMap") {

  type K = Int
  type V = Int
  type T = (K, V)

  property("internal underlying index match") = forAll { m: Map[K, V] =>
    !m.isEmpty ==> {
      val vm = VectorMap.from(m)
      val last = vm.keys.last
      vm.fields(vm.underlying(last)._1) == last
    }
  }

  property("internal underlying and field length") = forAll { m: Map[K, V] => {
      val vm = VectorMap.from(m)
      vm.underlying.size == vm.keys.length
    }
  }

  property("internal underlying and index are consistent after removal") = forAll { (m: Map[K, V]) =>
    m.size >= 3 ==> {
      val v = Vector.from(m)
      val random = v(new scala.util.Random().nextInt(v.size))
      val vm = VectorMap.from(v)
      val removed = vm - random._1
      removed.underlying.forall { case (k, (s, v)) => removed.fields(s) == k }
      removed.fields.zipWithIndex.forall {
        case (k: K, s) => removed.underlying(k)._1 == s
        case _ => true
      }
    }
  }
}
