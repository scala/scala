package scala.collection.immutable

import org.scalacheck._
import Arbitrary.arbitrary
import Prop._
import Gen._

object VectorMapProperties extends Properties("immutable.VectorMap") {
  property("internal underlying index match") = forAll { (m: Map[Int, Int]) =>
    !m.isEmpty ==> {
      val vm = VectorMap.from(m)
      val last = vm.fields.last
      vm.underlying(last)._1 == vm.size - 1
    }
  }

  property("internal underlying and field length") = forAll { (m: Map[Int, Int]) => {
      val vm = VectorMap.from(m)
      vm.underlying.size == vm.fields.length
    }
  }

}
