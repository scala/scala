package scala.collection.immutable

import org.scalacheck.Prop.forAll
import org.scalacheck._
import scala.reflect.ClassTag

object ArraySeqProperties extends Properties("immutable.ArraySeq") {

  private def sortedTest[A: Arbitrary: ClassTag: Ordering] = forAll{ (xs1: ArraySeq[A], x: A) =>
    val array1 = xs1.toArray
    assert(xs1.sorted == array1.sorted.to(ArraySeq))
    assert(xs1 == array1.to(ArraySeq))

    // check if internal `unsafeArray` is `Array[AnyRef]`
    val xs2 = xs1 :+ x
    assert(xs2.sorted == (array1 :+ x).sorted.to(ArraySeq))
    true
  }

  property("sorted Int") = sortedTest[Int]

  property("sorted Option[Int]") = sortedTest[Option[Int]]

}
