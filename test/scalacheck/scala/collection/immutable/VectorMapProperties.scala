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

  property("internal underlying and keys length") = forAll { m: Map[K, V] => {
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

  property("repeatedly removing to empty is empty at end") = forAll { (m: Map[K, V]) => {
      var vm = VectorMap.from(m)
      val sz1 = vm.size
      for (k <- vm.keys) vm = vm.removed(k)
      val sz2 = vm.size
      for (n <- 0 until sz1) vm = vm + (n -> n)
      val sz3 = vm.size
      for (k <- vm.keys) vm = vm.removed(k)
      vm.size == 0 && sz2 == 0 && sz3 == sz1
    }
  }

  property("nullable types for keys and values") = forAll { m: Map[String, String] => {
      val vm = VectorMap.from(m)
      vm.underlying.size == vm.keys.length
    }
  }

  property("internal underlying init size of non empty is consistent with original size and keys init size") = forAll { m: Map[K, V] =>
    m.size >= 1 ==> {
      val vm = VectorMap.from(m)
      val sz = vm.size - 1
      vm.underlying.init.size == sz && vm.keys.init.size == sz
    }
  }

}
