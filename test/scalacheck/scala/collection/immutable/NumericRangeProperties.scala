package scala
package collection.immutable

import org.scalacheck._

import scala.util.Try

class NumericRangeProperties extends Properties("immutable.NumericRange") {
  import Prop._
  import NumericRangeProperties._

  property("same length when take and drop with a specific amount (Byte)") = forAll { (r: NumericRange[Byte], amount: Int) =>
    Try(r.length).isSuccess ==> {
      r.take(amount).length + r.drop(amount).length == r.length
    }
  }

  property("same length when take and drop with a specific amount (Short)") = forAll { (r: NumericRange[Short], amount: Int) =>
    Try(r.length).isSuccess ==> {
      r.take(amount).length + r.drop(amount).length == r.length
    }
  }

  property("same length when take and drop with a specific amount (Int)") = forAll { (r: NumericRange[Int], amount: Int) =>
    Try(r.length).isSuccess ==> {
      r.take(amount).length + r.drop(amount).length == r.length
    }
  }

  property("same length when take and drop with a specific amount (Long)") = forAll { (r: NumericRange[Long], amount: Int) =>
    Try(r.length).isSuccess ==> {
      r.take(amount).length + r.drop(amount).length == r.length
    }
  }

  property("same length when take and drop with a specific amount (BigInt)") = forAll { (r: NumericRange[BigInt], amount: Int) =>
    Try(r.length).isSuccess ==> {
      r.take(amount).length + r.drop(amount).length == r.length
    }
  }
}

object NumericRangeProperties {
  private implicit def arbitraryNumericRange[T](implicit num: Integral[T], tGen: Arbitrary[T]): Arbitrary[NumericRange[T]] =
    Arbitrary(
      for {
        start <- tGen.arbitrary
        end <- tGen.arbitrary
        step <- tGen.arbitrary filterNot (num.equiv(_, num.zero))
        incl  <- Gen.oneOf(true, false)
      } yield if (incl) NumericRange.inclusive(start, end, step) else NumericRange(start, end, step)
    )
}