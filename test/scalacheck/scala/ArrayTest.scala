package scala

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import scala.util.Random

object ArrayTest extends Properties("Array") {

  property("range") = forAll(
    Gen.choose(-1000, 1000),
    Gen.choose(-1000, 1000),
    Gen.oneOf(Gen.choose(1, 100), Gen.choose(-100, -1)),
  ) { (start, end, step) =>
    Array.range(start, end, step).toList == List.range(start, end, step)
  }

  property("iterate") = forAll(
    implicitly[Arbitrary[Int]].arbitrary,
    Gen.choose(-10, 100),
    implicitly[Arbitrary[Int => Int]].arbitrary,
  ) { (start, len, f) =>
    Array.iterate(start, len)(f).toList == List.iterate(start, len)(f)
  }

  property("fill") = forAll(
    Gen.choose(-10, 100),
  ) { len =>
    val xs = Vector.fill(len)(Random.nextInt)
    val i = xs.iterator
    Array.fill(len)(i.next).toVector == xs
  }

  property("tabulate") = forAll(
    Gen.choose(-10, 100),
    implicitly[Arbitrary[Int => Int]].arbitrary,
  ) { (len, f) =>
    Array.tabulate(len)(f).toList == List.tabulate(len)(f)
  }

}
