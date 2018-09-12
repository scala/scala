package scala.collection.immutable

import org.scalacheck._
import Prop.forAll

object ListMapProps extends Properties("immutable.ListMap") {
  property("inits") = forAll { (m: ListMap[Byte, Byte]) =>
    assert(m.inits.map(_.size).toList == (m.size to 0 by -1))

    var y = m
    m.inits.foreach { x =>
      assert(x eq y)
      if (y.nonEmpty) {
        y = y.init
      }
    }

    m.inits.toList == m.toList.inits.map(_.to(ListMap)).toList
  }
}
