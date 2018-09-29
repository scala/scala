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
      val last = vm.fields.last
      vm.underlying(last)._1 == vm.size - 1
    }
  }

  property("internal underlying and field length") = forAll { m: Map[K, V] => {
      val vm = VectorMap.from(m)
      vm.underlying.size == vm.fields.length
    }
  }

  property("internal underlying has consistent index") = forAll { m: Map[K, V] =>
    m.size >= 3 ==> {
      val v = Vector.from(m)
      val random = v(new scala.util.Random().nextInt(v.size))
      val vm = VectorMap.from(v)
      val removed = vm - random._1
      removed.underlying.toList.map{case (k, v) => v._1}.sorted.sliding(2).forall(x => x(1) - x(0) == 1 )
    }
  }
}
